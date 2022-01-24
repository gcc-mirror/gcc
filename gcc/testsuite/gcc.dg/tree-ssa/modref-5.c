/* { dg-options "-O2 -fdump-tree-modref1"  } */
/* { dg-do run } */
__attribute__ ((noinline))
void
copy (int *a, int *b)
{
  *a=*b;
}
int p, *ptr = &p;
__attribute__ ((noinline))
void
barrier ()
{
  asm ("":"=r"(ptr):"0"(ptr));
}
int
main()
{
  int a = 1, b = 2;
  copy (&a,&b);
  barrier ();
  *ptr = 1;
  if (!__builtin_constant_p (b == 2))
    __builtin_abort ();
  return 0;
}
/* { dg-final { scan-tree-dump "parm 1 flags: no_direct_clobber no_direct_escape" "modref1"  } } */
