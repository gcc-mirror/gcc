/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-optimized" } */

struct C {
  long l;
  C():l(0) {}
};

int main()
{
  C* alloc;
  __transaction_atomic {
    alloc = new C;
  }
  alloc->l = 2;

  return 0;
}

/* { dg-final { scan-assembler-not "_ITM_getTMCloneOrIrrevocable" } } */
