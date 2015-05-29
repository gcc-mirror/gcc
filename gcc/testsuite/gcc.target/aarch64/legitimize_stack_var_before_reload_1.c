/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-expand" } */

extern void initialize_array (unsigned char *, int);

int
test15 (void)
{
  unsigned char a[480];

  initialize_array (a, 480);

  if (a[0] == 0x10)
    return 1;

  return 0;
}

/* { dg-final { scan-rtl-dump "\\(mem\[^\\n\]*\\(plus\[^\\n\]*virtual-stack-vars" "expand" } } */

