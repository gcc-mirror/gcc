/* PR target/39226 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mtune=cell -mminimal-toc" { target { powerpc*-*-* && lp64 } } } */

#if (__SIZEOF_INT__ == 2)	
struct A
{
  char *a;
  unsigned int b : 1;
  unsigned int c : 15;
};
#else
struct A
{
  char *a;
  unsigned int b : 1;
  unsigned int c : 31;
};
#endif

struct B
{
  struct A *d;
};

void
foo (struct B *x, unsigned long y)
{
  if (x->d[y].c)
    return;
  if (x->d[y].b)
    x->d[y].a = 0;
}
