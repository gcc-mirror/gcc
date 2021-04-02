/* PR c++/98413 - ICE on placement new and member pointer
   { dg-do compile }
   { dg-options "-Wall" } */

void* operator new (__SIZE_TYPE__, void *p) { return p; }

struct A { int m; } a;

void fc (int A::*p)
{
  new (&(a.*p)) char;
}

void fi (int A::*p)
{
  new (&(a.*p)) int;
}

void fB (int A::*p)
{
  struct B { int a[2]; };
  new (&(a.*p)) B;            // { dg-warning "\\\[-Wplacement-new" }
}
