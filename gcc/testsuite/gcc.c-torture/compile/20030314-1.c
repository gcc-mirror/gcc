/* PR optimization/8396 */
/* Originator: <papadopo@shfj.cea.fr> */

/* Verify that the tree inliner doesn't mess up the types
   when passing the value of read-only constant arguments.  */

static inline bar(const short int xs, const short int xe)
{
  if (xe && (xs < xe))
    ;
}
  
void f()
{
  short int xe;

  bar(0, xe);
}
