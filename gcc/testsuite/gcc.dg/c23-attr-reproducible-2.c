/* Test C23 reproducible attribute: invalid contexts.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

/* This attribute is not valid in most cases on types other than
   type specifiers with function type or function declarators.  */

[[reproducible]]; /* { dg-error "ignored" } */

int [[reproducible]] var; /* { dg-error "ignored" } */

int array_with_dep_type[2] [[reproducible]]; /* { dg-error "ignored" } */

[[reproducible]] int fn1 (); /* { dg-error "standard 'reproducible' attribute can only be applied to function declarators or type specifiers with function type" } */

[[reproducible]] int fn2 (), fn3 (); /* { dg-error "standard 'reproducible' attribute can only be applied to function declarators or type specifiers with function type" } */

int var2 [[reproducible]]; /* { dg-warning "'reproducible' attribute only applies to function types" } */

int fn4 [[reproducible]] (); /* { dg-error "standard 'reproducible' attribute can only be applied to function declarators or type specifiers with function type" } */

int [[reproducible]] fn5 (); /* { dg-error "ignored" } */

int z = sizeof (int [[__reproducible__]]); /* { dg-error "ignored" } */

/* This is valid, but not really useful, as it can't return results
   in return type nor has any pointer arguments to store results into.  */
void
fn6 (int x, double y) [[reproducible]]
{ /* { dg-warning "reproducible' attribute on function type without pointer arguments returning 'void'" } */
  y = x;
  (void) y;
}

void
f (void)
{
  int a;
  [[reproducible]]; /* { dg-error "ignored" } */
  [[reproducible]] a = 1; /* { dg-error "ignored" } */
  [[reproducible]] label: ; /* { dg-warning "'reproducible' attribute only applies to function types" } */
  switch (var)
    {
    [[reproducible]] case 1: ; /* { dg-warning "'reproducible' attribute only applies to function types" } */
    [[reproducible]] default: ; /* { dg-warning "'reproducible' attribute only applies to function types" } */
    }
}
