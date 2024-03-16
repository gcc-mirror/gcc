/* Test C23 support for empty initializers: invalid use cases with GNU
   extensions.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */

void
f (int a)
{
  /* Make sure a non-braced initializer for a VLA-in-struct is still not
     allowed.  */
  struct s { int x[a]; };
  struct s b;
  for (int i = 0; i < a; i++)
    b.x[i] = 0;
  struct s c = b; /* { dg-error "variable-sized object may not be initialized except with an empty initializer" } */
}
