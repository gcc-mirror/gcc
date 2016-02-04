/* { dg-do compile } */
/* { dg-options "-fdiagnostics-show-caret" } */

struct foo { int x; };
union u { int x; };

/* Verify that we issue a hint for "." used with a ptr to a struct.  */

int test_1 (struct foo *ptr)
{
  return ptr.x; /* { dg-error "'ptr' is a pointer; did you mean to use '->'?" } */
/* { dg-begin-multiline-output "" }
   return ptr.x;
             ^
             ->
   { dg-end-multiline-output "" } */
}

/* Likewise for a ptr to a union.  */

int test_2 (union u *ptr)
{
  return ptr.x; /* { dg-error "'ptr' is a pointer; did you mean to use '->'?" } */
/* { dg-begin-multiline-output "" }
   return ptr.x;
             ^
             ->
   { dg-end-multiline-output "" } */
}

/* Verify that we don't issue a hint for a ptr to something that isn't a
   struct or union.  */

int test_3 (void **ptr)
{
  return ptr.x; /* { dg-error "request for member 'x' in something not a structure or union" } */
/* { dg-begin-multiline-output "" }
   return ptr.x;
             ^
   { dg-end-multiline-output "" } */
}
