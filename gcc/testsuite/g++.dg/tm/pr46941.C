// { dg-do "compile" }
// { dg-options "-fgnu-tm" }

class Obj
{
  int dummy;
};

__attribute__((transaction_safe))
Obj* allocate()
{
  return new Obj;
}

__attribute__((transaction_safe))
void deallocate(Obj * o)
{
  delete o;
}

__attribute__((transaction_safe))
Obj* allocatearray()
{
  return new Obj[2];
}

__attribute__((transaction_safe))
void deallocatearray(Obj *o[])
{
  delete [] o;
}

/* The delete/new operators are handled by the libitm runtime.  */
/* { dg-final { scan-assembler "_ZGTtnw\[mj\]" } } */
/* { dg-final { scan-assembler "_ZGTtna\[mj\]" } } */
/* { dg-final { scan-assembler "_ZGTtdlPv" } } */
/* { dg-final { scan-assembler "_ZGTtdaPv" } } */
