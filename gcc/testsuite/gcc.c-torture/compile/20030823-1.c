struct A
{
  int a;
};

int foo (struct A *a)
{
  static int c = 30;
  int x;

  a->a = c;
  /* Dominator optimizations will replace the use of 'a->a' with 'c', but
     they won't copy the virtual operands for 'c' from its originating
     statement.  This exposes symbol 'c' without a correct SSA version
     number.  */
  x = a->a;
  return x;
}
