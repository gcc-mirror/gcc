/* Test for constant expressions: GNU extensions.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -pedantic-errors" } */

int n;

void
f (void)
{
  int i = 0;
  int a[n]; /* { dg-error "ISO C90 forbids variable length array" } */
  enum e1 {
    /* Integer constant expressions may not contain statement
       expressions (not a permitted operand).  */
    E1 = (1 ? 0 : ({ 0; })), /* { dg-error "constant expression" } */
    /* { dg-error "ISO C forbids braced-groups" "ISO" { target *-*-* } .-1 } */
    /* Real and imaginary parts act like other arithmetic
       operators.  */
    E2 = __real__ (1 ? 0 : i++), /* { dg-error "constant expression" } */
    E3 = __real__ 0,
    E4 = __imag__ (1 ? 0 : i++), /* { dg-error "constant" } */
    E5 = __imag__ 0,
    /* __alignof__ always constant.  */
    E6 = __alignof__ (int[n]), /* { dg-error "ISO C90 forbids variable length array" } */
    E7 = __alignof__ (a),
    /* __extension__ ignored for constant expression purposes.  */
    E8 = __extension__ (1 ? 0 : i++), /* { dg-error "constant expression" } */
    E9 = __extension__ 0,
    /* Conditional expressions with omitted arguments act like the
       standard type.  */ 
    E10 = (1 ? : i++), /* { dg-error "constant expression" } */
    /* { dg-error "ISO C forbids omitting" "ISO" { target *-*-* } .-1 } */
    E11 = (1 ? : 0) /* { dg-error "ISO C forbids omitting" } */
  };
  enum e2 {
    /* Complex integer constants may be cast directly to integer
       types, but not after further arithmetic on them.  */
    F1 = (int) (_Complex int) 2i, /* { dg-error "constant expression" } */
    /* { dg-error "complex" "complex" { target *-*-* } .-1 } */
    /* { dg-error "imaginary" "imaginary" { target *-*-* } .-2 } */
    F2 = (int) +2i, /* { dg-error "constant expression" } */
    /* { dg-error "imaginary" "ISO" { target *-*-* } .-1 } */
    F3 = (int) (1 + 2i), /* { dg-error "constant expression" } */
    /* { dg-error "imaginary" "ISO" { target *-*-* } .-1 } */
    F4 = (int) 2i /* { dg-error "imaginary" } */
  };
  static double dr = __real__ (1.0 + 2.0i);
  /* { dg-error "imaginary" "ISO" { target *-*-* } .-1 } */
  static double di = __imag__ (1.0 + 2.0i);
  /* { dg-error "imaginary" "ISO" { target *-*-* } .-1 } */
  /* Statement expressions allowed in unevaluated subexpressions in
     initializers in gnu99 but not gnu89.  */
  static int j = (1 ? 0 : ({ 0; })); /* { dg-error "constant expression" } */
  /* { dg-error "braced" "ISO" { target *-*-* } .-1 } */
}
