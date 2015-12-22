/* PR c/69002 */
/* Test we diagnose accessing elements of atomic structures or unions,
   which is undefined behavior (C11 6.5.2.3#5).  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

struct S { int x; };
union U { int x; };

int
fn1 (_Atomic struct S p)
{
  int e = 0 && p.x;
  return p.x + e; /* { dg-warning "accessing a member .x. of an atomic structure" } */
}

int
fn2 (_Atomic struct S *p)
{
  int e = 1 || p->x;
  return p->x + e; /* { dg-warning "accessing a member .x. of an atomic structure" } */
}

void
fn3 (_Atomic struct S p, int x)
{
  p.x = x; /* { dg-warning "accessing a member .x. of an atomic structure" } */
}

void
fn4 (_Atomic struct S *p, int x)
{
  p->x = x; /* { dg-warning "accessing a member .x. of an atomic structure" } */
}

int
fn5 (_Atomic struct S p)
{
  /* This is OK: Members can be safely accessed using a non-atomic
     object which is assigned to or from the atomic object.  */
  struct S s = p;
  return s.x;
}

int
fn6 (_Atomic struct S *p)
{
  struct S s = *p;
  return s.x;
}

int
fn7 (_Atomic union U p)
{
  int e = 0 && p.x;
  return p.x + e; /* { dg-warning "accessing a member .x. of an atomic union" } */
}

int
fn8 (_Atomic union U *p)
{
  int e = 1 || p->x;
  return p->x + e; /* { dg-warning "accessing a member .x. of an atomic union" } */
}

void
fn9 (_Atomic union U p, int x)
{
  p.x = x; /* { dg-warning "accessing a member .x. of an atomic union" } */
}

void
fn10 (_Atomic union U *p, int x)
{
  p->x = x; /* { dg-warning "accessing a member .x. of an atomic union" } */
}

int
fn11 (_Atomic union U p)
{
  /* This is OK: Members can be safely accessed using a non-atomic
     object which is assigned to or from the atomic object.  */
  union U s = p;
  return s.x;
}

int
fn12 (_Atomic union U *p)
{
  union U s = *p;
  return s.x;
}
