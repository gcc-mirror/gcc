/* PR middle-end/68582 */
/* { dg-do compile } */
/* { dg-options "-Wunused-function" } */

/* We failed to give the warning for functions with TREE_THIS_VOLATILE set.  */

static void
fn1 (void) /* { dg-warning "defined but not used" } */
{
  __builtin_abort ();
}

__attribute__ ((noreturn))
static void
fn2 (void) /* { dg-warning "defined but not used" } */
{
  __builtin_abort ();
}

__attribute__ ((volatile))
static void
fn3 (void) /* { dg-warning "defined but not used" } */
{
  __builtin_abort ();
}
