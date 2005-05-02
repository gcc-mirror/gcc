/* { dg-do compile { target powerpc*-*-* rs6000-*-* } } */
/* { dg-options "-mno-powerpc-gfxopt -mpowerpc64" } */
extern void bar (void *);
extern double x;
void
foo (void)
{
  char buf2 [32][1024];
  bar (buf2 [(int) x]);
}

