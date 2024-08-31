/* Test C23 unsequenced attribute: invalid contexts.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

/* This attribute is not valid in most cases on types other than
   type specifiers with function type or function declarators.  */

[[unsequenced]]; /* { dg-error "ignored" } */

int [[unsequenced]] var; /* { dg-error "ignored" } */

int array_with_dep_type[2] [[unsequenced]]; /* { dg-error "ignored" } */

[[unsequenced]] int fn1 (); /* { dg-error "standard 'unsequenced' attribute can only be applied to function declarators or type specifiers with function type" } */

[[unsequenced]] int fn2 (), fn3 (); /* { dg-error "standard 'unsequenced' attribute can only be applied to function declarators or type specifiers with function type" } */

int var2 [[unsequenced]]; /* { dg-warning "'unsequenced' attribute only applies to function types" } */

int fn4 [[unsequenced]] (); /* { dg-error "standard 'unsequenced' attribute can only be applied to function declarators or type specifiers with function type" } */

int [[unsequenced]] fn5 (); /* { dg-error "ignored" } */

int z = sizeof (int [[__unsequenced__]]); /* { dg-error "ignored" } */

/* This is valid, but not really useful, as it can't return results
   in return type nor has any pointer arguments to store results into.  */
void
fn6 (int x, double y) [[unsequenced]]
{ /* { dg-warning "unsequenced' attribute on function type without pointer arguments returning 'void'" } */
  y = x;
  (void) y;
}

void
f (void)
{
  int a;
  [[unsequenced]]; /* { dg-error "ignored" } */
  [[unsequenced]] a = 1; /* { dg-error "ignored" } */
  [[unsequenced]] label: ; /* { dg-warning "'unsequenced' attribute only applies to function types" } */
  switch (var)
    {
    [[unsequenced]] case 1: ; /* { dg-warning "'unsequenced' attribute only applies to function types" } */
    [[unsequenced]] default: ; /* { dg-warning "'unsequenced' attribute only applies to function types" } */
    }
}
