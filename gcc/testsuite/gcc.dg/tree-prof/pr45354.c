/* { dg-require-effective-target freorder } */
/* { dg-options "-O2 -freorder-blocks-and-partition -fschedule-insns -fselective-scheduling" { target powerpc*-*-* ia64-*-* x86_64-*-* } } */

extern void abort (void);

int ifelse_val2;

int __attribute__((noinline))
test_ifelse2 (int i)
{
  int result = 0;
  if (!i)				/* count(6) */
    result = 1;				/* count(1) */
  if (i == 1)				/* count(6) */
    result = 1024;
  if (i == 2)				/* count(6) */
    result = 2;				/* count(3) */
  if (i == 3)				/* count(6) */
    return 8;				/* count(2) */
  if (i == 4)				/* count(4) */
    return 2048;
  return result;			/* count(4) */
}

void __attribute__((noinline))
call_ifelse ()
{
  ifelse_val2 += test_ifelse2 (0);
  ifelse_val2 += test_ifelse2 (2);
  ifelse_val2 += test_ifelse2 (2);
  ifelse_val2 += test_ifelse2 (2);
  ifelse_val2 += test_ifelse2 (3);
  ifelse_val2 += test_ifelse2 (3);
}

int
main()
{
  call_ifelse ();
  if (ifelse_val2 != 23)
    abort ();
  return 0;
}
