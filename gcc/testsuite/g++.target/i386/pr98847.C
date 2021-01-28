// PR c++/98847
// { dg-do run }
// { dg-options "-O2 -masm=att" }

template <int N>
int
foo ()
{
  register int edx asm ("edx");
  asm ("movl $1234, %%edx" : "=r" (edx));
  return edx;
}

int
main ()
{
  if (foo<0> () != 1234)
    __builtin_abort ();
  return 0;
}
