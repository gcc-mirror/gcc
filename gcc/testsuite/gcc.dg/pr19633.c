/* { dg-do link } */
/* { dg-options "-O2" } */

struct S
{
  int w, x, y, z;
};

struct T
{
  int r;
  struct S s;
};

void
foo (int a, struct T b)
{
  struct S *c = 0;

  if (a)
    c = &b.s;

  b.s.w = 3;

  /* Since 'c' may be pointing to NULL here, we used to flag it as
     pointing anywhere, which was forcing the aliaser to mark as
     call-clobbered every other variable pointed-to by 'c' ('b' in
     this case).  This, in turn, caused the insertion of V_MAY_DEFs
     for 'b' at this call-site, which prevented constant propagation
     from 'b.s.w = 3' to 'if (b.s.w != 3)'.  */
  bar (*c, a);

  if (b.s.w != 3)
    link_error ();
}

int main ()
{
  struct T b;
  foo (3, b);
  return 0;
}

int X;

int bar (struct S x, int i)
{
  X = 3;
}
