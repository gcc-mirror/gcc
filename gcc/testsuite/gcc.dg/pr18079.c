/* PR c/18079 */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

__attribute__ ((noinline))
__attribute__ ((always_inline))
int
fn1 (int r)
{ /* { dg-warning "ignoring attribute .always_inline. because it conflicts with attribute .noinline." } */
  return r & 4;
}

__attribute__ ((noinline, always_inline))
int
fn2 (int r)
{ /* { dg-warning "ignoring attribute .always_inline. because it conflicts with attribute .noinline." } */
  return r & 4;
}

__attribute__ ((always_inline))
__attribute__ ((noinline))
inline int
fn3 (int r)
{ /* { dg-warning "ignoring attribute .noinline. because it conflicts with attribute .always_inline." } */
  return r & 8;
}

__attribute__ ((always_inline, noinline))
inline int
fn4 (int r)
{ /* { dg-warning "ignoring attribute .noinline. because it conflicts with attribute .always_inline." } */
  return r & 8;
}
