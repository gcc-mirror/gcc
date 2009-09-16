/* Test operation of -Wbad-function-cast.  */
/* Based on gcc.dg/Wbad-function-cast-1.c.  */

/* { dg-do compile } */
/* { dg-options "-Wbad-function-cast" } */

int if1(void);
char if2(void);
long if3(void);
float rf1(void);
double rf2(void);
_Decimal32 rf3(void);
_Decimal64 rf4(void);
_Decimal128 rf5(void);
_Complex double cf(void);

void
foo(void)
{
  /* Casts to void types are always OK.  */
  (void)rf3();
  (void)rf4();
  (void)rf5();
  (const void)rf3();
  /* Casts to the same type or similar types are OK.  */
  (_Decimal32)rf1();
  (_Decimal64)rf2();
  (_Decimal128)rf3();
  (_Decimal128)rf4();
  (_Decimal128)rf5();
  (float)rf3();
  (double)rf4();
  (long double)rf5();
   /* Casts to types with different TREE_CODE (which is how this
     warning has been defined) are not OK, except for casts to void
     types.  */
  (_Decimal32)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Decimal32'" } */
  (_Decimal64)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Decimal64'" } */
  (_Decimal128)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Decimal128'" } */
  (int)rf3(); /* { dg-warning "cast from function call of type '_Decimal32' to non-matching type 'int'" } */
  (long)rf4(); /* { dg-warning "cast from function call of type '_Decimal64' to non-matching type 'long int'" } */
  (long int)rf5(); /* { dg-warning "cast from function call of type '_Decimal128' to non-matching type 'long int'" } */
  (_Decimal32)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Decimal32'" } */
}
