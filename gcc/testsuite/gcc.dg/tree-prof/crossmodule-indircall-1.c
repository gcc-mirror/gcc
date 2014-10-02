/* { dg-require-effective-target lto } */
/* { dg-additional-sources "crossmodule-indircall-1a.c" } */
/* { dg-options "-O3 -flto -DDOJOB=1" } */

int a;
extern void (*p[2])(int n);
void abort (void);
int
main()
{ int i;

  /* This call shall be converted.  */
  for (i = 0;i<1000;i++)
    p[0](1);
  /* This call shall not be converted.  */
  for (i = 0;i<1000;i++)
    p[i%2](2);
  if (a != 1000)
    abort ();

  return 0;
}
