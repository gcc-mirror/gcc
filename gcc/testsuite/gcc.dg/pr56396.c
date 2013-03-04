/* PR tree-optimization/56396 */
/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fpic -g" } */

struct S { char *s; int z; };
struct T { int t; } *c, u;
void bar (int, const char *);

inline void *
foo (void *x, char *y, int z)
{
  struct S s;
  char b[256];
  s.s = b;
  s.z = __builtin___sprintf_chk (s.s, 1, __builtin_object_size (s.s, 2), "Require");
  if (s.z < 0)
    bar (u.t | c->t, "rls");
  if (foo (x, s.s, s.z))
    {
    }
  return (void *) 0;
}
