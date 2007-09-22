/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* N1169 6.5.7 - Bitwise shift operands.
   C99 6.5.3 Unary operators.
   C99 6.5.5 Multiplicative operators.
   C99 6.5.6 Additive operators.
   C99 6.5.7 Bitwise shift operators.
   C99 6.5.8 Relational operators.
   C99 6.5.9 Equality operators.
   C99 6.5.16 Assignment operators.

   Check if all operations are ok.  */

void true();
void false();

#define ALLOP(TYPE,NAME) \
  TYPE test1 ## NAME (TYPE a)  { return +a; } \
  TYPE test2 ## NAME (TYPE a)  { return -a; } \
  int test3 ## NAME (TYPE a)  { return !a; } \
  TYPE test4 ## NAME (TYPE a, TYPE b) { return a + b; } \
  TYPE test5 ## NAME (TYPE a, TYPE b) { return a - b; } \
  TYPE test6 ## NAME (TYPE a, TYPE b) { return a * b; } \
  TYPE test7 ## NAME (TYPE a, TYPE b) { return a / b; } \
  TYPE test8 ## NAME (TYPE a, TYPE b) { a += b; return a; } \
  TYPE test9 ## NAME (TYPE a, TYPE b) { a -= b; return a; } \
  TYPE test10 ## NAME (TYPE a, TYPE b) { a *= b; return a; } \
  TYPE test11 ## NAME (TYPE a, TYPE b) { a /= b; return a; } \
  TYPE test12 ## NAME (TYPE a, int b) { return a << b; } \
  TYPE test13 ## NAME (TYPE a, int b) { return a >> b; } \
  TYPE test14 ## NAME (TYPE a, int b) { a <<= b; return a; } \
  TYPE test15 ## NAME (TYPE a, int b) { a >>= b; return a; } \
  int test16 ## NAME (TYPE a, TYPE b) { return a == b; } \
  int test17 ## NAME (TYPE a, TYPE b) { return a != b; } \
  int test18 ## NAME (TYPE a, TYPE b) { return a < b; } \
  int test19 ## NAME (TYPE a, TYPE b) { return a <= b; } \
  int test20 ## NAME (TYPE a, TYPE b) { return a >= b; } \
  int test21 ## NAME (TYPE a, TYPE b) { return a > b; } \
  void test22 ## NAME (TYPE a, TYPE b) { if(a == b)true(); else false(); } \
  void test23 ## NAME (TYPE a, TYPE b) { if(a != b)true(); else false(); } \
  void test24 ## NAME (TYPE a, TYPE b) { if(a < b)true(); else false(); } \
  void test25 ## NAME (TYPE a, TYPE b) { if(a <= b)true(); else false(); } \
  void test26 ## NAME (TYPE a, TYPE b) { if(a >= b)true(); else false(); } \
  void test27 ## NAME (TYPE a, TYPE b) { if(a > b)true(); else false(); } \
  TYPE test28 ## NAME (TYPE a) { return ++a; } \
  TYPE test29 ## NAME (TYPE a) { return --a; } \
  TYPE test30 ## NAME (TYPE *a) { return ++(*a); } \
  TYPE test31 ## NAME (TYPE *a) { return (*a)++; } \
  TYPE test32 ## NAME (TYPE *a) { return --(*a); } \
  TYPE test33 ## NAME (TYPE *a) { return (*a)--; }

ALLOP(short _Fract, sf);
ALLOP(_Fract, f);
ALLOP(long _Fract, lf);
ALLOP(long long _Fract, llf);
ALLOP(short _Accum, sa);
ALLOP(_Accum, a);
ALLOP(long _Accum, la);
ALLOP(long long _Accum, lla);

ALLOP(unsigned short _Fract, usf);
ALLOP(unsigned _Fract, uf);
ALLOP(unsigned long _Fract, ulf);
ALLOP(unsigned long long _Fract, ullf);
ALLOP(unsigned short _Accum, usa);
ALLOP(unsigned _Accum, ua);
ALLOP(unsigned long _Accum, ula);
ALLOP(unsigned long long _Accum, ulla);

ALLOP(_Sat short _Fract, Ssf);
ALLOP(_Sat _Fract, Sf);
ALLOP(_Sat long _Fract, Slf);
ALLOP(_Sat long long _Fract, Sllf);
ALLOP(_Sat short _Accum, Ssa);
ALLOP(_Sat _Accum, Sa);
ALLOP(_Sat long _Accum, Sla);
ALLOP(_Sat long long _Accum, Slla);

ALLOP(_Sat unsigned short _Fract, Susf);
ALLOP(_Sat unsigned  _Fract, Suf);
ALLOP(_Sat unsigned long _Fract, Sulf);
ALLOP(_Sat unsigned long long _Fract, Sullf);
ALLOP(_Sat unsigned short _Accum, Susa);
ALLOP(_Sat unsigned  _Accum, Sua);
ALLOP(_Sat unsigned long _Accum, Sula);
ALLOP(_Sat unsigned long long _Accum, Sulla);
