/* Function definitions that are used by multiple tests.  */

#define CHECKS(NAME,TYPEM)					\
void checkS##NAME##1 (S##NAME##1 x, TYPEM y)			\
{ if (x.a != y) DEBUG_CHECK }					\
void checkS##NAME##2 (S##NAME##2 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 ) DEBUG_CHECK }			\
void checkS##NAME##3 (S##NAME##3 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 )			\
    DEBUG_CHECK }						\
void checkS##NAME##4 (S##NAME##4 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3)	\
    DEBUG_CHECK }						\
void checkS##NAME##5 (S##NAME##5 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4) DEBUG_CHECK }				\
void checkS##NAME##6 (S##NAME##6 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5) DEBUG_CHECK }		\
void checkS##NAME##7 (S##NAME##7 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6)		\
    DEBUG_CHECK }						\
void checkS##NAME##8 (S##NAME##8 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7) DEBUG_CHECK }				\
void checkS##NAME##9 (S##NAME##9 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8) DEBUG_CHECK }		\
void checkS##NAME##10 (S##NAME##10 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8 || x.j != y+9)		\
   DEBUG_CHECK }						\
void checkS##NAME##11 (S##NAME##11 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8 || x.j != y+9			\
      || x.k != y+10) DEBUG_CHECK }				\
void checkS##NAME##12 (S##NAME##12 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8 || x.j != y+9			\
      || x.k != y+10 || x.l != y+11) DEBUG_CHECK }		\
void checkS##NAME##13 (S##NAME##13 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8 || x.j != y+9			\
      || x.k != y+10 || x.l != y+11 || x.m != y+12)		\
    DEBUG_CHECK }						\
void checkS##NAME##14 (S##NAME##14 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8 || x.j != y+9			\
      || x.k != y+10 || x.l != y+11 || x.m != y+12		\
      || x.n != y+13) DEBUG_CHECK }				\
void checkS##NAME##15 (S##NAME##15 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8 || x.j != y+9			\
      || x.k != y+10 || x.l != y+11 || x.m != y+12		\
      || x.n != y+13 || x.o != y+14) DEBUG_CHECK }		\
void checkS##NAME##16 (S##NAME##16 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8 || x.j != y+9			\
      || x.k != y+10 || x.l != y+11 || x.m != y+12		\
      || x.n != y+13 || x.o != y+14 || x.p != y+15)		\
    DEBUG_CHECK }
