/* Origin: PR c/179 from Gray Watson <gray@256.com>, adapted as a testcase
   by Joseph Myers <jsm28@cam.ac.uk>.  */
/* -ftrivial-auto-var-init will make the uninitialized warning for address
   taken auto var going away, FIXME later.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized -ftrivial-auto-var-init=zero" } */
extern void foo (int *);
extern void bar (int);

void
baz (void)
{
  int i;
  if (i) /* { dg-warning "is used uninitialized" "uninit i warning" { xfail *-*-* } }  */
    bar (i);
  foo (&i);
}
