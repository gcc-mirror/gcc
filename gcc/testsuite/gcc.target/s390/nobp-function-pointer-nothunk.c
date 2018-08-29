/* { dg-do compile } */
/* { dg-options "-O3  -march=z10 --save-temps -mindirect-branch-call=thunk-extern -mindirect-branch-table" } */

int gl;

void __attribute__((noinline,noclone))
foo (int a)
{
  gl = a + 40;
}

int __attribute__((noinline,noclone))
foo_value (int a)
{
  return a + 40;
}

void*  __attribute__((noinline,noclone))
get_fptr (int a)
{
  switch (a)
    {
    case 0: return &foo; break;
    case 1: return &foo_value; break;
    default: __builtin_abort ();
    }
}

void (*f) (int);
int (*g) (int);

int
main ()
{
  int res;

  f = get_fptr(0);
  f (2);
  if (gl != 42)
    __builtin_abort ();

  g = get_fptr(1);
  if (g (2) != 42)
    __builtin_abort ();

  return 0;
}

/* 2 x main
/* { dg-final { scan-assembler-times "brasl\t%r\[0-9\]*,__s390_indirect_jump" 2 } } */

/* No thunks due to thunk-extern.  */
/* { dg-final { scan-assembler-not "exrl" } } */
/* { dg-final { scan-assembler-not ".globl __s390_indirect_jump" } } */

/* { dg-final { scan-assembler-not "section\t.s390_indirect_jump" } } */
/* { dg-final { scan-assembler     "section\t.s390_indirect_call" } } */
/* { dg-final { scan-assembler-not "section\t.s390_return_reg" } } */
/* { dg-final { scan-assembler-not "section\t.s390_return_mem" } } */
