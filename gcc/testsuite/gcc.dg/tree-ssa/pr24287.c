/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-optimized" } */
int g1(int);
int h(int *a, int *b)__attribute__((pure));
void link_error();

/* The calls to link_error should be eliminated, since nothing escapes to 
   non-pure functions.  */
int g(void)
{
  int t = 0, t1 = 2;
  /* ???  That's not true.  The pointers escape to the integer return
     value which we do not track in PTA.  */
  int t2 = h(&t, &t1);
  if (t != 0)
    link_error ();
  if (t1 != 2)
    link_error ();
  /* ???  And it would finally escape here even if we did.  */
  g1(t2);
  if (t != 0)
    link_error ();
  if (t1 != 2)
    link_error ();
  return t2 == 2;
}
/* We are allowed to optimize the first two link_error calls.  */
/* { dg-final { scan-tree-dump-times "link_error" 2 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
