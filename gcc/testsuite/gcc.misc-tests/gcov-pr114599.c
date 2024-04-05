/* PR middle-end/114599 */
/* { dg-do compile } */
/* { dg-options "-fcondition-coverage" } */

/* Check that a function with a condition inlined into a function without a
   conditional works.  When inlining happens the condition -> expression
   mapping must be carried over.  */

extern int type;

void fn (void);

__attribute__((always_inline))
inline void
do_all_fn_doall_arg (void)
{
  if (type)
    fn ();
}

void
do_all_fn_LHASH_DOALL_ARG_arg2 (void)
{
  do_all_fn_doall_arg ();
}
