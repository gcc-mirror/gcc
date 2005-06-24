/* Test that the tree_nrv pass works by making sure that we don't generate
   a memcpy.  Throw in a bit of control flow to make its job a bit harder.  */

/* { dg-options "-O" } */

struct A { int i[100]; };

int b;

struct A f ()
{
  struct A a;
  if (b)
    {
      a.i[0] = 42;
      return a;
    }
  else
    {
      a.i[42] = 1;
      return a;
    }
}

/* { dg-final { scan-assembler-not "memcpy" } } */
