/* Test operation of -Wbad-function-cast.  Bug 6980 complained of the
   wording of the diagnostic.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-Wbad-function-cast" } */

void vf(void);
int if1(void);
char if2(void);
long if3(void);
float rf1(void);
double rf2(void);
_Complex double cf(void);
enum e { E1 } ef(void);
_Bool bf(void);
char *pf1(void);
int *pf2(void);

void
foo(void)
{
  /* Casts to void types are always OK.  */
  (void)vf();
  (void)if1();
  (void)cf();
  (const void)bf();
  /* Casts to the same type or similar types are OK.  */
  (int)if1();
  (long)if2();
  (char)if3();
  (float)rf1();
  (long double)rf2();
  (_Complex float)cf();
  (enum f { F1 })ef();
  (_Bool)bf();
  (void *)pf1();
  (char *)pf2();
  /* Casts to types with different TREE_CODE (which is how this
     warning has been defined) are not OK, except for casts to void
     types.  */
  (float)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'float'" } */
  (double)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'double'" } */
  (_Bool)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Bool'" } */
  (int)rf1(); /* { dg-warning "cast from function call of type 'float' to non-matching type 'int'" } */
  (long)rf2(); /* { dg-warning "cast from function call of type 'double' to non-matching type 'long int'" } */
  (double)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'double'" } */
  (int)ef(); /* { dg-warning "cast from function call of type 'enum e' to non-matching type 'int'" } */
  (int)bf(); /* { dg-warning "cast from function call of type '_Bool' to non-matching type 'int'" } */
  (__SIZE_TYPE__)pf1(); /* { dg-warning "cast from function call of type 'char \\*' to non-matching type '\[^\\n\]*'" } */
  (__PTRDIFF_TYPE__)pf2(); /* { dg-warning "cast from function call of type 'int \\*' to non-matching type '\[^\\n\]*'" } */
}
