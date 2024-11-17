/* { dg-do compile } */
/* { dg-options "-std=gnu17 -Wdeprecated-non-prototype -Wno-declaration-missing-parameter-type" } */

void f1 (not_a_type); /* { dg-note "declared here" } */
void f2 (not_a_type); /* { dg-note "declared here" } */

void
g ()
{
  /* This is not ideal, but the GCC type system does not capture the number of
     arguments in a non-prototype function declaration.  */
  f1 (1); /* { dg-warning "does not allow arguments for function" } */
}

void
f2 (int not_a_type) /* { dg-warning "does not allow defining parameters" } */
{
}
