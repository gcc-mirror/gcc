/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* C99 6.5.4 Cast operators.
   Test invalid casts involving decimal float.  */

struct s { _Decimal32 d32; } sv;
union u { _Decimal32 d32; } uv;

_Decimal32 d32;
_Decimal64 d64;
_Decimal128 d128;

_Decimal32* d32p;
_Decimal64* d64p;
_Decimal128* d128p;

void
f (void)
{
  (_Decimal32 []) d32p; /* { dg-error "cast specifies array type" } */
  (_Decimal32 ()) d32p; /* { dg-error "cast specifies function type" } */
  (_Decimal64 []) d64p; /* { dg-error "cast specifies array type" } */
  (_Decimal64 ()) d64p; /* { dg-error "cast specifies function type" } */
  (_Decimal128 []) d128p; /* { dg-error "cast specifies array type" } */
  (_Decimal128 ()) d128p; /* { dg-error "cast specifies function type" } */
  
  (struct s) d32; /* { dg-error "conversion to non-scalar type requested" } */
  (union u) d32;
  (struct s) d64; /* { dg-error "conversion to non-scalar type requested" } */
  (union u) d64; /* { dg-error "cast to union type from type not present in union" } */
  (struct s) d128; /* { dg-error "conversion to non-scalar type requested" } */
  (union u) d128; /* { dg-error "cast to union type from type not present in union" } */
		
  (_Decimal32) sv; /* { dg-error "aggregate value used where a float was expected" } */
  (_Decimal32) uv; /* { dg-error "aggregate value used where a float was expected" } */
  (_Decimal64) sv; /* { dg-error "aggregate value used where a float was expected" } */
  (_Decimal64) uv; /* { dg-error "aggregate value used where a float was expected" } */
  (_Decimal128) sv; /* { dg-error "aggregate value used where a float was expected" } */
  (_Decimal128) uv; /* { dg-error "aggregate value used where a float was expected" } */
}
