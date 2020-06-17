/* Test ICE with designated initializer in compound literal with bad
   type name (ICE seen with early version of fix for bug 93577 but not
   covered in other tests).  */
/* { dg-do compile } */
/* { dg-options "" } */

void *
f (void)
{
  return &(const bad_type) { .a = 0 }; /* { dg-error "unknown type name" } */
}
