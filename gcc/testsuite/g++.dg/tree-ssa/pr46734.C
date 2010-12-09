/* { dg-do compile } */
/* { dg-options "-O -fipa-sra" } */

struct A
{
  int *p;
  A() {p = (int *) -1;}
  ~A() {if (p && p != (int *) -1) *p = 0;}
};

struct B
{
  A a;
  char data[23];
  B() : a() {data[0] = 0;}
};

extern A ga;
extern int *gi;
extern void *gz;
extern B *gb;

static int * __attribute__ ((noinline)) foo (B *b, void *z)
{
  __builtin_memcpy (gz, z, 28);
  ga = b->a;
  return b->a.p;
}

int *bar (B *b, void *z)
{
  gb = b;
  return foo (b, z);
}
