/* { dg-do compile } */
/* { dg-options "-fpermissive -O -Wall -ftrack-macro-expansion=0" } */

/* C99 6.2.7: Compatible type and composite type.  */

#define DECIMAL_COMPOSITE_DECL(TYPE) \
  _Decimal##TYPE g1_##TYPE(); \
  _Decimal##TYPE g2_##TYPE(); \
  _Decimal##TYPE (*h1_##TYPE)[2]; \
  _Decimal##TYPE (*h2_##TYPE)[3]; \
  _Decimal##TYPE (*h3_##TYPE)[4]; \
  _Decimal##TYPE f1_##TYPE(_Decimal##TYPE(*)()); \
  _Decimal##TYPE f1_##TYPE(_Decimal##TYPE(*)(_Decimal##TYPE*)); \
  _Decimal##TYPE f1_##TYPE (_Decimal##TYPE(*g)(_Decimal##TYPE*)) \
   { \
     _Decimal##TYPE d##TYPE; \
     d##TYPE = ((_Decimal##TYPE (*) (_Decimal##TYPE*)) g)(&d##TYPE); \
     d##TYPE = ((_Decimal##TYPE (*) ()) g); \
     return d##TYPE; \
   } \
   _Decimal##TYPE f2_##TYPE(_Decimal##TYPE(*)[]); \
   _Decimal##TYPE f2_##TYPE(_Decimal##TYPE(*)[3]);

#define DECIMAL_COMPOSITE_TEST(TYPE) \
do \
{ \
 _Decimal##TYPE d##TYPE; \
 d##TYPE = f1_##TYPE(g1_##TYPE); \
 d##TYPE = f1_##TYPE(g2_##TYPE); \
 d##TYPE = f2_##TYPE(h1_##TYPE); \
 d##TYPE = f2_##TYPE(h2_##TYPE); \
 d##TYPE = f2_##TYPE(h3_##TYPE); \
 (void) d##TYPE; \
} while(0)
 
DECIMAL_COMPOSITE_DECL(32);  /* { dg-error "incompatible types when assigning to type '\[^\n\]*' from type '\[^\n\]*'" } */
/* { dg-message "note: expected '\[^'\n\]*' but argument is of type '\[^'\n\]*'" "note: expected" { target *-*-* } .-1 } */


DECIMAL_COMPOSITE_DECL(64);  /* { dg-error "incompatible types when assigning to type '\[^\n\]*' from type '\[^\n\]*'" } */
/* { dg-message "note: expected '\[^'\n\]*' but argument is of type '\[^'\n\]*'" "note: expected" { target *-*-* } .-1 } */


DECIMAL_COMPOSITE_DECL(128); /* { dg-error "incompatible types when assigning to type '\[^\n\]*' from type '\[^\n\]*'" } */
/* { dg-message "note: expected '\[^'\n\]*' but argument is of type '\[^'\n\]*'" "note: expected" { target *-*-* } .-1 } */


int main()
{
  DECIMAL_COMPOSITE_TEST(32);  /* { dg-warning "incompatible pointer type" } */
  DECIMAL_COMPOSITE_TEST(64);  /* { dg-warning "incompatible pointer type" } */
  DECIMAL_COMPOSITE_TEST(128); /* { dg-warning "incompatible pointer type" } */

  return 0;
}

/* The invalid function redeclarations might also trigger:
   { dg-prune-output "-Warray-parameter" } */
