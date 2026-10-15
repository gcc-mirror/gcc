/* PR tree-optimization/128013 */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-psabi" } */

typedef int v4si __attribute__ ((vector_size (16)));
extern int rt (void) __attribute__ ((returns_twice));
extern void bar (void);
extern void use (v4si);

/* v occurs in abnormal PHIs.  The CONSTRUCTOR must not become a use of v
   after its redefinition.  */

void
f (v4si *out, v4si v)
{
  int a0 = v[0], a1 = v[1], a2 = v[2], a3 = v[3];
  bar ();
  v = v * 3;
  *out = (v4si) { a0, a1, a2, a3 };
  bar ();
  rt ();
  use (v);
}
