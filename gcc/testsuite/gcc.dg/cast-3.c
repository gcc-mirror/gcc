/* Test diagnostics for bad or doubtful casts.  Test with
   -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic-errors" } */

struct s { int a; } sv;
union u { int a; } uv;
int i;
long l;
char c;
void *p;
float fv;

void
f (void)
{
  (int []) p; /* { dg-error "cast specifies array type" } */
  (int ()) p; /* { dg-error "cast specifies function type" } */
  (struct s) sv; /* { dg-error "ISO C forbids casting nonscalar to the same type" } */
  (union u) uv; /* { dg-error "ISO C forbids casting nonscalar to the same type" } */
  (struct s) i; /* { dg-error "conversion to non-scalar type requested" } */
  (union u) i; /* { dg-error "ISO C forbids casts to union type" } */
  (union u) l; /* { dg-error "cast to union type from type not present in union" } */
  (int) sv; /* { dg-error "aggregate value used where an integer was expected" } */
  (int) uv; /* { dg-error "aggregate value used where an integer was expected" } */
  (float) sv; /* { dg-error "aggregate value used where a float was expected" } */
  (float) uv; /* { dg-error "aggregate value used where a float was expected" } */
  (_Complex double) sv; /* { dg-error "aggregate value used where a complex was expected" } */
  (_Complex double) uv; /* { dg-error "aggregate value used where a complex was expected" } */
  (void *) sv; /* { dg-error "cannot convert to a pointer type" } */
  (void *) uv; /* { dg-error "cannot convert to a pointer type" } */
  (_Bool) sv; /* { dg-error "used struct type value where scalar is required" } */
  (_Bool) uv; /* { dg-error "used union type value where scalar is required" } */
  (void) sv;
  (const void) uv;
  (void *) c; /* { dg-warning "cast to pointer from integer of different size" } */
  (void *) (char) 1;
  (char) p; /* { dg-warning "cast from pointer to integer of different size" } */
  (char) (void *) 1; /* { dg-warning "cast from pointer to integer of different size" } */
}
