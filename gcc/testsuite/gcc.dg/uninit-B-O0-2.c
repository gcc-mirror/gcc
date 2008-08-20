/* Origin: PR c/179 inverse of uninit-B-O0.c, we should not warn.  */
/* { dg-do compile } */
/* { dg-options "-O0 -Wuninitialized" } */
extern void foo (int *);
extern void bar (int);

void
baz (void)
{
  int i;
  foo (&i); /* { dg-bogus "is used uninitialized" "uninit i warning" } */
  if (i) 
    bar (i);
}


