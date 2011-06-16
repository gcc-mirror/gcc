/* PR target/49307 */
/* { dg-do compile } */
/* { dg-options "-O -fpic -fstack-protector" } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target fstack_protector } */

extern void bar (char **pp, void *vp);
extern void free (void *p);

int
foo (void)
{
  char *p;
  char fext[128];

  p = fext;
  bar (&p, (void *)0);
  if (p)
    free (p);
  return 0;
}
