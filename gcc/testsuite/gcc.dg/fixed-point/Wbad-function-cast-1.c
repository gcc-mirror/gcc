/* Test operation of -Wbad-function-cast.  */
/* Based on gcc.dg/Wbad-function-cast-1.c.  */

/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wbad-function-cast" } */

int if1(void);
char if2(void);
long if3(void);
_Complex double cf(void);


#define FUNC(TYPE,NAME) \
TYPE f ## NAME (void);

FUNC (short _Fract, sf)
FUNC (_Fract, f)
FUNC (long _Fract, lf)
FUNC (long long _Fract, llf)
FUNC (unsigned short _Fract, usf)
FUNC (unsigned _Fract, uf)
FUNC (unsigned long _Fract, ulf)
FUNC (unsigned long long _Fract, ullf)
FUNC (_Sat short _Fract, Ssf)
FUNC (_Sat _Fract, Sf)
FUNC (_Sat long _Fract, Slf)
FUNC (_Sat long long _Fract, Sllf)
FUNC (_Sat unsigned short _Fract, Susf)
FUNC (_Sat unsigned _Fract, Suf)
FUNC (_Sat unsigned long _Fract, Sulf)
FUNC (_Sat unsigned long long _Fract, Sullf)
FUNC (short _Accum, sa)
FUNC (_Accum, a)
FUNC (long _Accum, la)
FUNC (long long _Accum, lla)
FUNC (unsigned short _Accum, usa)
FUNC (unsigned _Accum, ua)
FUNC (unsigned long _Accum, ula)
FUNC (unsigned long long _Accum, ulla)
FUNC (_Sat short _Accum, Ssa)
FUNC (_Sat _Accum, Sa)
FUNC (_Sat long _Accum, Sla)
FUNC (_Sat long long _Accum, Slla)
FUNC (_Sat unsigned short _Accum, Susa)
FUNC (_Sat unsigned _Accum, Sua)
FUNC (_Sat unsigned long _Accum, Sula)
FUNC (_Sat unsigned long long _Accum, Sulla)

void
foo(void)
{
#define TEST(NAME) \
  /* Casts to void types are always OK.  */ \
  (void) f ## NAME (); \
  (const void) f ## NAME (); \
  /* Casts to the same type or similar types are OK.  */ \
  (short _Fract) f ## NAME (); \
  (_Fract) f ## NAME (); \
  (long _Fract) f ## NAME (); \
  (long long _Fract) f ## NAME (); \
  (unsigned short _Fract) f ## NAME (); \
  (unsigned _Fract) f ## NAME (); \
  (unsigned long _Fract) f ## NAME (); \
  (unsigned long long _Fract) f ## NAME (); \
  (_Sat short _Fract) f ## NAME (); \
  (_Sat _Fract) f ## NAME (); \
  (_Sat long _Fract) f ## NAME (); \
  (_Sat long long _Fract) f ## NAME (); \
  (_Sat unsigned short _Fract) f ## NAME (); \
  (_Sat unsigned _Fract) f ## NAME (); \
  (_Sat unsigned long _Fract) f ## NAME (); \
  (_Sat unsigned long long _Fract) f ## NAME (); \
  (short _Accum) f ## NAME (); \
  (_Accum) f ## NAME (); \
  (long _Accum) f ## NAME (); \
  (long long _Accum) f ## NAME (); \
  (unsigned short _Accum) f ## NAME (); \
  (unsigned _Accum) f ## NAME (); \
  (unsigned long _Accum) f ## NAME (); \
  (unsigned long long _Accum) f ## NAME (); \
  (_Sat short _Accum) f ## NAME (); \
  (_Sat _Accum) f ## NAME (); \
  (_Sat long _Accum) f ## NAME (); \
  (_Sat long long _Accum) f ## NAME (); \
  (_Sat unsigned short _Accum) f ## NAME (); \
  (_Sat unsigned _Accum) f ## NAME (); \
  (_Sat unsigned long _Accum) f ## NAME (); \
  (_Sat unsigned long long _Accum) f ## NAME (); \

  TEST (sf);
  TEST (f);
  TEST (lf);
  TEST (llf);
  TEST (usf);
  TEST (uf);
  TEST (ulf);
  TEST (ullf);
  TEST (Ssf);
  TEST (Sf);
  TEST (Slf);
  TEST (Sllf);
  TEST (Susf);
  TEST (Suf);
  TEST (Sulf);
  TEST (Sullf);
  TEST (sa);
  TEST (a);
  TEST (la);
  TEST (lla);
  TEST (usa);
  TEST (ua);
  TEST (ula);
  TEST (ulla);
  TEST (Ssa);
  TEST (Sa);
  TEST (Sla);
  TEST (Slla);
  TEST (Susa);
  TEST (Sua);
  TEST (Sula);
  TEST (Sulla);

   /* Casts to types with different TREE_CODE (which is how this
     warning has been defined) are not OK, except for casts to void
     types.  */
  (short _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'short _Fract'" } */
  (_Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Fract'" } */
  (long _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'long _Fract'" } */
  (long long _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'long long _Fract'" } */
  (unsigned short _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'unsigned short _Fract'" } */
  (unsigned _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'unsigned _Fract'" } */
  (unsigned long _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'unsigned long _Fract'" } */
  (unsigned long long _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'unsigned long long _Fract'" } */
  (_Sat short _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat short _Fract'" } */
  (_Sat _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat _Fract'" } */
  (_Sat long _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat long _Fract'" } */
  (_Sat long long _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat long long _Fract'" } */
  (_Sat unsigned short _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat unsigned short _Fract'" } */
  (_Sat unsigned _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat unsigned _Fract'" } */
  (_Sat unsigned long _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat unsigned long _Fract'" } */
  (_Sat unsigned long long _Fract)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat unsigned long long _Fract'" } */
  (short _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'short _Accum'" } */
  (_Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Accum'" } */
  (long _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'long _Accum'" } */
  (long long _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'long long _Accum'" } */
  (unsigned short _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'unsigned short _Accum'" } */
  (unsigned _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'unsigned _Accum'" } */
  (unsigned long _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'unsigned long _Accum'" } */
  (unsigned long long _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type 'unsigned long long _Accum'" } */
  (_Sat short _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat short _Accum'" } */
  (_Sat _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat _Accum'" } */
  (_Sat long _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat long _Accum'" } */
  (_Sat long long _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat long long _Accum'" } */
  (_Sat unsigned short _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat unsigned short _Accum'" } */
  (_Sat unsigned _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat unsigned _Accum'" } */
  (_Sat unsigned long _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat unsigned long _Accum'" } */
  (_Sat unsigned long long _Accum)if1(); /* { dg-warning "cast from function call of type 'int' to non-matching type '_Sat unsigned long long _Accum'" } */

  (short _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'short _Fract'" } */
  (_Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Fract'" } */
  (long _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'long _Fract'" } */
  (long long _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'long long _Fract'" } */
  (unsigned short _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'unsigned short _Fract'" } */
  (unsigned _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'unsigned _Fract'" } */
  (unsigned long _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'unsigned long _Fract'" } */
  (unsigned long long _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'unsigned long long _Fract'" } */
  (_Sat short _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat short _Fract'" } */
  (_Sat _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat _Fract'" } */
  (_Sat long _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat long _Fract'" } */
  (_Sat long long _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat long long _Fract'" } */
  (_Sat unsigned short _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat unsigned short _Fract'" } */
  (_Sat unsigned _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat unsigned _Fract'" } */
  (_Sat unsigned long _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat unsigned long _Fract'" } */
  (_Sat unsigned long long _Fract)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat unsigned long long _Fract'" } */
  (short _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'short _Accum'" } */
  (_Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Accum'" } */
  (long _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'long _Accum'" } */
  (long long _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'long long _Accum'" } */
  (unsigned short _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'unsigned short _Accum'" } */
  (unsigned _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'unsigned _Accum'" } */
  (unsigned long _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'unsigned long _Accum'" } */
  (unsigned long long _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type 'unsigned long long _Accum'" } */
  (_Sat short _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat short _Accum'" } */
  (_Sat _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat _Accum'" } */
  (_Sat long _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat long _Accum'" } */
  (_Sat long long _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat long long _Accum'" } */
  (_Sat unsigned short _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat unsigned short _Accum'" } */
  (_Sat unsigned _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat unsigned _Accum'" } */
  (_Sat unsigned long _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat unsigned long _Accum'" } */
  (_Sat unsigned long long _Accum)if2(); /* { dg-warning "cast from function call of type 'char' to non-matching type '_Sat unsigned long long _Accum'" } */

  (short _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type 'short _Fract'" } */
  (_Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Fract'" } */
  (long _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type 'long _Fract'" } */
  (long long _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type 'long long _Fract'" } */
  (unsigned short _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type 'unsigned short _Fract'" } */
  (unsigned _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type 'unsigned _Fract'" } */
  (unsigned long _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type 'unsigned long _Fract'" } */
  (unsigned long long _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type 'unsigned long long _Fract'" } */
  (_Sat short _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat short _Fract'" } */
  (_Sat _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat _Fract'" } */
  (_Sat long _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat long _Fract'" } */
  (_Sat long long _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat long long _Fract'" } */
  (_Sat unsigned short _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat unsigned short _Fract'" } */
  (_Sat unsigned _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat unsigned _Fract'" } */
  (_Sat unsigned long _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat unsigned long _Fract'" } */
  (_Sat unsigned long long _Fract)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat unsigned long long _Fract'" } */
  (short _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type 'short _Accum'" } */
  (_Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Accum'" } */
  (long _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type 'long _Accum'" } */
  (long long _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type 'long long _Accum'" } */
  (unsigned short _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type 'unsigned short _Accum'" } */
  (unsigned _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type 'unsigned _Accum'" } */
  (unsigned long _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type 'unsigned long _Accum'" } */
  (unsigned long long _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type 'unsigned long long _Accum'" } */
  (_Sat short _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat short _Accum'" } */
  (_Sat _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat _Accum'" } */
  (_Sat long _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat long _Accum'" } */
  (_Sat long long _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat long long _Accum'" } */
  (_Sat unsigned short _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat unsigned short _Accum'" } */
  (_Sat unsigned _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat unsigned _Accum'" } */
  (_Sat unsigned long _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat unsigned long _Accum'" } */
  (_Sat unsigned long long _Accum)if3(); /* { dg-warning "cast from function call of type 'long int' to non-matching type '_Sat unsigned long long _Accum'" } */

  (short _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'short _Fract'" } */
  (_Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Fract'" } */
  (long _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'long _Fract'" } */
  (long long _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'long long _Fract'" } */
  (unsigned short _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'unsigned short _Fract'" } */
  (unsigned _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'unsigned _Fract'" } */
  (unsigned long _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'unsigned long _Fract'" } */
  (unsigned long long _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'unsigned long long _Fract'" } */
  (_Sat short _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat short _Fract'" } */
  (_Sat _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat _Fract'" } */
  (_Sat long _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat long _Fract'" } */
  (_Sat long long _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat long long _Fract'" } */
  (_Sat unsigned short _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat unsigned short _Fract'" } */
  (_Sat unsigned _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat unsigned _Fract'" } */
  (_Sat unsigned long _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat unsigned long _Fract'" } */
  (_Sat unsigned long long _Fract)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat unsigned long long _Fract'" } */
  (short _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'short _Accum'" } */
  (_Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Accum'" } */
  (long _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'long _Accum'" } */
  (long long _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'long long _Accum'" } */
  (unsigned short _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'unsigned short _Accum'" } */
  (unsigned _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'unsigned _Accum'" } */
  (unsigned long _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'unsigned long _Accum'" } */
  (unsigned long long _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type 'unsigned long long _Accum'" } */
  (_Sat short _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat short _Accum'" } */
  (_Sat _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat _Accum'" } */
  (_Sat long _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat long _Accum'" } */
  (_Sat long long _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat long long _Accum'" } */
  (_Sat unsigned short _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat unsigned short _Accum'" } */
  (_Sat unsigned _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat unsigned _Accum'" } */
  (_Sat unsigned long _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat unsigned long _Accum'" } */
  (_Sat unsigned long long _Accum)cf(); /* { dg-warning "cast from function call of type 'complex double' to non-matching type '_Sat unsigned long long _Accum'" } */

  (int)fsf(); /* { dg-warning "cast from function call of type 'short _Fract' to non-matching type 'int'" } */
  (int)ff(); /* { dg-warning "cast from function call of type '_Fract' to non-matching type 'int'" } */
  (int)flf(); /* { dg-warning "cast from function call of type 'long _Fract' to non-matching type 'int'" } */
  (int)fllf(); /* { dg-warning "cast from function call of type 'long long _Fract' to non-matching type 'int'" } */
  (int)fusf(); /* { dg-warning "cast from function call of type 'unsigned short _Fract' to non-matching type 'int'" } */
  (int)fuf(); /* { dg-warning "cast from function call of type 'unsigned _Fract' to non-matching type 'int'" } */
  (int)fulf(); /* { dg-warning "cast from function call of type 'unsigned long _Fract' to non-matching type 'int'" } */
  (int)fullf(); /* { dg-warning "cast from function call of type 'unsigned long long _Fract' to non-matching type 'int'" } */
  (int)fSsf(); /* { dg-warning "cast from function call of type '_Sat short _Fract' to non-matching type 'int'" } */
  (int)fSf(); /* { dg-warning "cast from function call of type '_Sat _Fract' to non-matching type 'int'" } */
  (int)fSlf(); /* { dg-warning "cast from function call of type '_Sat long _Fract' to non-matching type 'int'" } */
  (int)fSllf(); /* { dg-warning "cast from function call of type '_Sat long long _Fract' to non-matching type 'int'" } */
  (int)fSusf(); /* { dg-warning "cast from function call of type '_Sat unsigned short _Fract' to non-matching type 'int'" } */
  (int)fSuf(); /* { dg-warning "cast from function call of type '_Sat unsigned _Fract' to non-matching type 'int'" } */
  (int)fSulf(); /* { dg-warning "cast from function call of type '_Sat unsigned long _Fract' to non-matching type 'int'" } */
  (int)fSullf(); /* { dg-warning "cast from function call of type '_Sat unsigned long long _Fract' to non-matching type 'int'" } */
  (int)fsa(); /* { dg-warning "cast from function call of type 'short _Accum' to non-matching type 'int'" } */
  (int)fa(); /* { dg-warning "cast from function call of type '_Accum' to non-matching type 'int'" } */
  (int)fla(); /* { dg-warning "cast from function call of type 'long _Accum' to non-matching type 'int'" } */
  (int)flla(); /* { dg-warning "cast from function call of type 'long long _Accum' to non-matching type 'int'" } */
  (int)fusa(); /* { dg-warning "cast from function call of type 'unsigned short _Accum' to non-matching type 'int'" } */
  (int)fua(); /* { dg-warning "cast from function call of type 'unsigned _Accum' to non-matching type 'int'" } */
  (int)fula(); /* { dg-warning "cast from function call of type 'unsigned long _Accum' to non-matching type 'int'" } */
  (int)fulla(); /* { dg-warning "cast from function call of type 'unsigned long long _Accum' to non-matching type 'int'" } */
  (int)fSsa(); /* { dg-warning "cast from function call of type '_Sat short _Accum' to non-matching type 'int'" } */
  (int)fSa(); /* { dg-warning "cast from function call of type '_Sat _Accum' to non-matching type 'int'" } */
  (int)fSla(); /* { dg-warning "cast from function call of type '_Sat long _Accum' to non-matching type 'int'" } */
  (int)fSlla(); /* { dg-warning "cast from function call of type '_Sat long long _Accum' to non-matching type 'int'" } */
  (int)fSusa(); /* { dg-warning "cast from function call of type '_Sat unsigned short _Accum' to non-matching type 'int'" } */
  (int)fSua(); /* { dg-warning "cast from function call of type '_Sat unsigned _Accum' to non-matching type 'int'" } */
  (int)fSula(); /* { dg-warning "cast from function call of type '_Sat unsigned long _Accum' to non-matching type 'int'" } */
  (int)fSulla(); /* { dg-warning "cast from function call of type '_Sat unsigned long long _Accum' to non-matching type 'int'" } */

  (long)fsf(); /* { dg-warning "cast from function call of type 'short _Fract' to non-matching type 'long int'" } */
  (long)ff(); /* { dg-warning "cast from function call of type '_Fract' to non-matching type 'long int'" } */
  (long)flf(); /* { dg-warning "cast from function call of type 'long _Fract' to non-matching type 'long int'" } */
  (long)fllf(); /* { dg-warning "cast from function call of type 'long long _Fract' to non-matching type 'long int'" } */
  (long)fusf(); /* { dg-warning "cast from function call of type 'unsigned short _Fract' to non-matching type 'long int'" } */
  (long)fuf(); /* { dg-warning "cast from function call of type 'unsigned _Fract' to non-matching type 'long int'" } */
  (long)fulf(); /* { dg-warning "cast from function call of type 'unsigned long _Fract' to non-matching type 'long int'" } */
  (long)fullf(); /* { dg-warning "cast from function call of type 'unsigned long long _Fract' to non-matching type 'long int'" } */
  (long)fSsf(); /* { dg-warning "cast from function call of type '_Sat short _Fract' to non-matching type 'long int'" } */
  (long)fSf(); /* { dg-warning "cast from function call of type '_Sat _Fract' to non-matching type 'long int'" } */
  (long)fSlf(); /* { dg-warning "cast from function call of type '_Sat long _Fract' to non-matching type 'long int'" } */
  (long)fSllf(); /* { dg-warning "cast from function call of type '_Sat long long _Fract' to non-matching type 'long int'" } */
  (long)fSusf(); /* { dg-warning "cast from function call of type '_Sat unsigned short _Fract' to non-matching type 'long int'" } */
  (long)fSuf(); /* { dg-warning "cast from function call of type '_Sat unsigned _Fract' to non-matching type 'long int'" } */
  (long)fSulf(); /* { dg-warning "cast from function call of type '_Sat unsigned long _Fract' to non-matching type 'long int'" } */
  (long)fSullf(); /* { dg-warning "cast from function call of type '_Sat unsigned long long _Fract' to non-matching type 'long int'" } */
  (long)fsa(); /* { dg-warning "cast from function call of type 'short _Accum' to non-matching type 'long int'" } */
  (long)fa(); /* { dg-warning "cast from function call of type '_Accum' to non-matching type 'long int'" } */
  (long)fla(); /* { dg-warning "cast from function call of type 'long _Accum' to non-matching type 'long int'" } */
  (long)flla(); /* { dg-warning "cast from function call of type 'long long _Accum' to non-matching type 'long int'" } */
  (long)fusa(); /* { dg-warning "cast from function call of type 'unsigned short _Accum' to non-matching type 'long int'" } */
  (long)fua(); /* { dg-warning "cast from function call of type 'unsigned _Accum' to non-matching type 'long int'" } */
  (long)fula(); /* { dg-warning "cast from function call of type 'unsigned long _Accum' to non-matching type 'long int'" } */
  (long)fulla(); /* { dg-warning "cast from function call of type 'unsigned long long _Accum' to non-matching type 'long int'" } */
  (long)fSsa(); /* { dg-warning "cast from function call of type '_Sat short _Accum' to non-matching type 'long int'" } */
  (long)fSa(); /* { dg-warning "cast from function call of type '_Sat _Accum' to non-matching type 'long int'" } */
  (long)fSla(); /* { dg-warning "cast from function call of type '_Sat long _Accum' to non-matching type 'long int'" } */
  (long)fSlla(); /* { dg-warning "cast from function call of type '_Sat long long _Accum' to non-matching type 'long int'" } */
  (long)fSusa(); /* { dg-warning "cast from function call of type '_Sat unsigned short _Accum' to non-matching type 'long int'" } */
  (long)fSua(); /* { dg-warning "cast from function call of type '_Sat unsigned _Accum' to non-matching type 'long int'" } */
  (long)fSula(); /* { dg-warning "cast from function call of type '_Sat unsigned long _Accum' to non-matching type 'long int'" } */
  (long)fSulla(); /* { dg-warning "cast from function call of type '_Sat unsigned long long _Accum' to non-matching type 'long int'" } */

  (long int)fsf(); /* { dg-warning "cast from function call of type 'short _Fract' to non-matching type 'long int'" } */
  (long int)ff(); /* { dg-warning "cast from function call of type '_Fract' to non-matching type 'long int'" } */
  (long int)flf(); /* { dg-warning "cast from function call of type 'long _Fract' to non-matching type 'long int'" } */
  (long int)fllf(); /* { dg-warning "cast from function call of type 'long long _Fract' to non-matching type 'long int'" } */
  (long int)fusf(); /* { dg-warning "cast from function call of type 'unsigned short _Fract' to non-matching type 'long int'" } */
  (long int)fuf(); /* { dg-warning "cast from function call of type 'unsigned _Fract' to non-matching type 'long int'" } */
  (long int)fulf(); /* { dg-warning "cast from function call of type 'unsigned long _Fract' to non-matching type 'long int'" } */
  (long int)fullf(); /* { dg-warning "cast from function call of type 'unsigned long long _Fract' to non-matching type 'long int'" } */
  (long int)fSsf(); /* { dg-warning "cast from function call of type '_Sat short _Fract' to non-matching type 'long int'" } */
  (long int)fSf(); /* { dg-warning "cast from function call of type '_Sat _Fract' to non-matching type 'long int'" } */
  (long int)fSlf(); /* { dg-warning "cast from function call of type '_Sat long _Fract' to non-matching type 'long int'" } */
  (long int)fSllf(); /* { dg-warning "cast from function call of type '_Sat long long _Fract' to non-matching type 'long int'" } */
  (long int)fSusf(); /* { dg-warning "cast from function call of type '_Sat unsigned short _Fract' to non-matching type 'long int'" } */
  (long int)fSuf(); /* { dg-warning "cast from function call of type '_Sat unsigned _Fract' to non-matching type 'long int'" } */
  (long int)fSulf(); /* { dg-warning "cast from function call of type '_Sat unsigned long _Fract' to non-matching type 'long int'" } */
  (long int)fSullf(); /* { dg-warning "cast from function call of type '_Sat unsigned long long _Fract' to non-matching type 'long int'" } */
  (long int)fsa(); /* { dg-warning "cast from function call of type 'short _Accum' to non-matching type 'long int'" } */
  (long int)fa(); /* { dg-warning "cast from function call of type '_Accum' to non-matching type 'long int'" } */
  (long int)fla(); /* { dg-warning "cast from function call of type 'long _Accum' to non-matching type 'long int'" } */
  (long int)flla(); /* { dg-warning "cast from function call of type 'long long _Accum' to non-matching type 'long int'" } */
  (long int)fusa(); /* { dg-warning "cast from function call of type 'unsigned short _Accum' to non-matching type 'long int'" } */
  (long int)fua(); /* { dg-warning "cast from function call of type 'unsigned _Accum' to non-matching type 'long int'" } */
  (long int)fula(); /* { dg-warning "cast from function call of type 'unsigned long _Accum' to non-matching type 'long int'" } */
  (long int)fulla(); /* { dg-warning "cast from function call of type 'unsigned long long _Accum' to non-matching type 'long int'" } */
  (long int)fSsa(); /* { dg-warning "cast from function call of type '_Sat short _Accum' to non-matching type 'long int'" } */
  (long int)fSa(); /* { dg-warning "cast from function call of type '_Sat _Accum' to non-matching type 'long int'" } */
  (long int)fSla(); /* { dg-warning "cast from function call of type '_Sat long _Accum' to non-matching type 'long int'" } */
  (long int)fSlla(); /* { dg-warning "cast from function call of type '_Sat long long _Accum' to non-matching type 'long int'" } */
  (long int)fSusa(); /* { dg-warning "cast from function call of type '_Sat unsigned short _Accum' to non-matching type 'long int'" } */
  (long int)fSua(); /* { dg-warning "cast from function call of type '_Sat unsigned _Accum' to non-matching type 'long int'" } */
  (long int)fSula(); /* { dg-warning "cast from function call of type '_Sat unsigned long _Accum' to non-matching type 'long int'" } */
  (long int)fSulla(); /* { dg-warning "cast from function call of type '_Sat unsigned long long _Accum' to non-matching type 'long int'" } */

}
