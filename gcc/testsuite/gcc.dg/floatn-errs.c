/* Tests for _FloatN / _FloatNx types: test erroneous code.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu17" } */
/* { dg-add-options float32 } */
/* { dg-add-options float64 } */
/* { dg-add-options float32x } */
/* { dg-require-effective-target float32 } */
/* { dg-require-effective-target float32x } */
/* { dg-require-effective-target float64 } */

/* _FloatN, _FloatNx and standard types are incompatible even if they
   have the same ABI.  */

extern float a; /* { dg-message "previous declaration" } */
extern _Float32 a; /* { dg-error "conflicting" } */

extern double b; /* { dg-message "previous declaration" } */
extern _Float32x b; /* { dg-error "conflicting" } */

extern _Float64 c; /* { dg-message "previous declaration" } */
extern _Float32x c; /* { dg-error "conflicting" } */

/* These types are not promoted in old-style function definitions.  */

void f (_Float32);
void
f (x)
     _Float32 x;
{
}

void g (double); /* { dg-error "prototype declaration" } */
void
g (x)
     _Float32 x; /* { dg-error "match prototype" } */
{
}

void h (_Float64); /* { dg-error "prototype declaration" } */
void
h (x)
     _Float32 x; /* { dg-error "match prototype" } */
{
}
