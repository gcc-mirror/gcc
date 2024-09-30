/* { dg-do compile { target { struct_musttail && { c || c++11 } } } } */

extern int foo2 (int x, ...);

struct str
{
  int a, b;
};

struct str
cstruct (int x)
{
  if (x < 10)
    [[clang::musttail]] return cstruct (x + 1);
  return ((struct str){ x, 0 });
}

int
foo (int x)
{
  if (x < 10)
    [[clang::musttail]] return foo2 (x, 29);
  if (x < 100)
    {
      int k = foo (x + 1);
      [[clang::musttail]] return k;	/* { dg-error "cannot tail-call: " } */
    }
  return x;
}
