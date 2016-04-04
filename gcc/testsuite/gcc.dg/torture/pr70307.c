/* PR c/70307 */
/* { dg-do compile } */

typedef int v4si __attribute__ ((vector_size (16)));

v4si foo (v4si);

v4si
fn1 (int i)
{
  return i <= (v4si){(0, 0)};
}

v4si
fn2 (int i)
{
  v4si r;
  r = i <= (v4si){(0, 0)};
  return r;
}

v4si
fn3 (int i)
{
  return foo (i <= (v4si){(0, 0)});
}

v4si
fn4 (int i)
{
  struct S { v4si v; };
  struct S s = { .v = i <= (v4si){(0, 0)} };
  return s.v;
}

v4si
fn5 (int i)
{
  return (v4si){(1, i++)} == (v4si){(0, 0)};
}

v4si
fn6 (int i)
{
  v4si r;
  r = (v4si){(1, i++)} == (v4si){(0, 0)};
  return r;
}

v4si
fn7 (int i)
{
  return foo ((v4si){(1, i++)} == (v4si){(0, 0)});
}

v4si
fn8 (int i)
{
  struct S { v4si v; };
  struct S s = { .v = (v4si){(1, i++)} == (v4si){(0, 0)} };
  return s.v;
}
