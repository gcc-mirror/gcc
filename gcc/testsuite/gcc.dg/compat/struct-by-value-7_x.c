#ifdef DBG
#include <stdio.h>
#define DEBUG_FPUTS(x) fputs (x, stdout)
#define DEBUG_DOT putc ('.', stdout)
#define DEBUG_NL putc ('\n', stdout)
#else
#define DEBUG_FPUTS(x)
#define DEBUG_DOT
#define DEBUG_NL
#endif

#define X(NAME,TYPEM)						\
typedef struct { TYPEM a; } S##NAME##1;				\
typedef struct { TYPEM a; TYPEM b; } S##NAME##2;		\
typedef struct { TYPEM a; TYPEM b; TYPEM c; } S##NAME##3;	\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; }		\
	       S##NAME##4;					\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e; }	\
	       S##NAME##5;					\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
  		 TYPEM f; } S##NAME##6;				\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; } S##NAME##7;		\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; } S##NAME##8;	\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; }		\
	       S##NAME##9;					\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; TYPEM j; }	\
	       S##NAME##10;					\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; TYPEM j;	\
		 TYPEM k; } S##NAME##11;			\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; TYPEM j;	\
		 TYPEM k; TYPEM l; } S##NAME##12;		\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; TYPEM j;	\
		 TYPEM k; TYPEM l; TYPEM m; } S##NAME##13;	\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; TYPEM j;	\
		 TYPEM k; TYPEM l; TYPEM m; TYPEM n; }		\
	        S##NAME##14;					\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; TYPEM j;	\
		 TYPEM k; TYPEM l; TYPEM m; TYPEM n; TYPEM o; }	\
	       S##NAME##15;					\
typedef struct { TYPEM a; TYPEM b; TYPEM c; TYPEM d; TYPEM e;	\
		 TYPEM f; TYPEM g; TYPEM h; TYPEM i; TYPEM j;	\
		 TYPEM k; TYPEM l; TYPEM m; TYPEM n; TYPEM o;	\
		 TYPEM p; } S##NAME##16;			\
								\
void checkS##NAME##1 (S##NAME##1 x, TYPEM y)			\
{ if (x.a != y) { DEBUG_NL; DEBUG_NL; abort (); } }		\
void checkS##NAME##2 (S##NAME##2 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 ) { DEBUG_NL; abort (); } }	\
void checkS##NAME##3 (S##NAME##3 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 )			\
    { DEBUG_NL; abort (); } }					\
void checkS##NAME##4 (S##NAME##4 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3)	\
    { DEBUG_NL; abort (); } }					\
void checkS##NAME##5 (S##NAME##5 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4) { DEBUG_NL; abort (); } }			\
void checkS##NAME##6 (S##NAME##6 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5) { DEBUG_NL; abort (); } }	\
void checkS##NAME##7 (S##NAME##7 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6)		\
    { DEBUG_NL; abort (); } }					\
void checkS##NAME##8 (S##NAME##8 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7) { DEBUG_NL; abort (); } }			\
void checkS##NAME##9 (S##NAME##9 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8) { DEBUG_NL; abort (); } }	\
void checkS##NAME##10 (S##NAME##10 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8 || x.j != y+9)		\
   { DEBUG_NL; abort (); } }					\
void checkS##NAME##11 (S##NAME##11 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8 || x.j != y+9			\
      || x.k != y+10) { DEBUG_NL; abort (); } }			\
void checkS##NAME##12 (S##NAME##12 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8 || x.j != y+9			\
      || x.k != y+10 || x.l != y+11) { DEBUG_NL; abort (); } }	\
void checkS##NAME##13 (S##NAME##13 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8 || x.j != y+9			\
      || x.k != y+10 || x.l != y+11 || x.m != y+12)		\
    { DEBUG_NL; abort (); } }					\
void checkS##NAME##14 (S##NAME##14 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8 || x.j != y+9			\
      || x.k != y+10 || x.l != y+11 || x.m != y+12		\
      || x.n != y+13) { DEBUG_NL; abort (); } }			\
void checkS##NAME##15 (S##NAME##15 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8 || x.j != y+9			\
      || x.k != y+10 || x.l != y+11 || x.m != y+12		\
      || x.n != y+13 || x.o != y+14) { DEBUG_NL; abort (); } }	\
void checkS##NAME##16 (S##NAME##16 x, TYPEM y)			\
{ if (x.a != y || x.b != y+1 || x.c != y+2 || x.d != y+3	\
      || x.e != y+4 || x.f != y+5 || x.g != y+6			\
      || x.h != y+7 || x.i != y+8 || x.j != y+9			\
      || x.k != y+10 || x.l != y+11 || x.m != y+12		\
      || x.n != y+13 || x.o != y+14 || x.p != y+15)		\
    { DEBUG_NL; abort (); } }

#define T(TYPE,MTYPE)						\
TYPE g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE;		\
TYPE g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE;		\
TYPE g9s##TYPE, g10s##TYPE, g11s##TYPE, g12s##TYPE;		\
TYPE g13s##TYPE, g14s##TYPE, g15s##TYPE, g16s##TYPE;		\
								\
extern void init##TYPE (TYPE *p, MTYPE x);			\
extern void checkg##TYPE (void);				\
extern void							\
test##TYPE (TYPE s1, TYPE s2, TYPE s3, TYPE s4,			\
	    TYPE s5, TYPE s6, TYPE s7, TYPE s8,			\
	    TYPE s9, TYPE s10, TYPE s11, TYPE s12,		\
	    TYPE s13, TYPE s14, TYPE s15, TYPE s16);		\
extern void testva##TYPE (int n, ...);				\
								\
void								\
test2_##TYPE (TYPE s1, TYPE s2, TYPE s3, TYPE s4,		\
	      TYPE s5, TYPE s6, TYPE s7, TYPE s8)		\
{								\
  test##TYPE (s1, g2s##TYPE, s2, g4s##TYPE,			\
	      s3, g6s##TYPE, s4, g8s##TYPE,			\
	      s5, g10s##TYPE, s6, g12s##TYPE,			\
	      s7, g14s##TYPE, s8, g16s##TYPE);			\
}								\
								\
void								\
testit##TYPE (void)						\
{								\
  DEBUG_FPUTS (#TYPE);						\
  init##TYPE  ( &g1s##TYPE,  (MTYPE)1);				\
  init##TYPE  ( &g2s##TYPE,  (MTYPE)2);				\
  init##TYPE  ( &g3s##TYPE,  (MTYPE)3);				\
  init##TYPE  ( &g4s##TYPE,  (MTYPE)4);				\
  init##TYPE  ( &g5s##TYPE,  (MTYPE)5);				\
  init##TYPE  ( &g6s##TYPE,  (MTYPE)6);				\
  init##TYPE  ( &g7s##TYPE,  (MTYPE)7);				\
  init##TYPE  ( &g8s##TYPE,  (MTYPE)8);				\
  init##TYPE  ( &g9s##TYPE,  (MTYPE)9);				\
  init##TYPE  (&g10s##TYPE, (MTYPE)10);				\
  init##TYPE  (&g11s##TYPE, (MTYPE)11);				\
  init##TYPE  (&g12s##TYPE, (MTYPE)12);				\
  init##TYPE  (&g13s##TYPE, (MTYPE)13);				\
  init##TYPE  (&g14s##TYPE, (MTYPE)14);				\
  init##TYPE  (&g15s##TYPE, (MTYPE)15);				\
  init##TYPE  (&g16s##TYPE, (MTYPE)16);				\
  checkg##TYPE ();						\
  DEBUG_FPUTS (" test");					\
  test##TYPE (g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE,	\
	      g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE,	\
	      g9s##TYPE, g10s##TYPE, g11s##TYPE, g12s##TYPE,	\
	      g13s##TYPE, g14s##TYPE, g15s##TYPE, g16s##TYPE);	\
  DEBUG_FPUTS (" testva");					\
  testva##TYPE (1,						\
		g1s##TYPE);					\
  testva##TYPE (2,						\
		g1s##TYPE, g2s##TYPE);				\
  testva##TYPE (3,						\
		g1s##TYPE, g2s##TYPE, g3s##TYPE);		\
  testva##TYPE (4,						\
		g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE);	\
  testva##TYPE (5,						\
		g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE,	\
		g5s##TYPE);					\
  testva##TYPE (6,						\
		g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE,	\
		g5s##TYPE, g6s##TYPE);				\
  testva##TYPE (7,						\
		g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE,	\
		g5s##TYPE, g6s##TYPE, g7s##TYPE);		\
  testva##TYPE (8,						\
		g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE,	\
		g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE);	\
  testva##TYPE (9,						\
		g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE,	\
		g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE,	\
		g9s##TYPE);					\
  testva##TYPE (10,						\
		g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE,	\
		g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE,	\
		g9s##TYPE, g10s##TYPE);				\
  testva##TYPE (11,						\
		g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE,	\
		g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE,	\
		g9s##TYPE, g10s##TYPE, g11s##TYPE);		\
  testva##TYPE (12,						\
		g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE,	\
		g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE,	\
		g9s##TYPE, g10s##TYPE, g11s##TYPE, g12s##TYPE);	\
  testva##TYPE (13,						\
		g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE,	\
		g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE,	\
		g9s##TYPE, g10s##TYPE, g11s##TYPE, g12s##TYPE,	\
		g13s##TYPE);					\
  testva##TYPE (14,						\
		g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE,	\
		g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE,	\
		g9s##TYPE, g10s##TYPE, g11s##TYPE, g12s##TYPE,	\
		g13s##TYPE, g14s##TYPE);			\
  testva##TYPE (15,						\
		g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE,	\
		g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE,	\
		g9s##TYPE, g10s##TYPE, g11s##TYPE, g12s##TYPE,	\
		g13s##TYPE, g14s##TYPE, g15s##TYPE);		\
  testva##TYPE (16,						\
		g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE,	\
		g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE,	\
		g9s##TYPE, g10s##TYPE, g11s##TYPE, g12s##TYPE,	\
		g13s##TYPE, g14s##TYPE, g15s##TYPE, g16s##TYPE); \
  DEBUG_FPUTS (" test2");					\
  test2_##TYPE (g1s##TYPE, g3s##TYPE, g5s##TYPE, g7s##TYPE,	\
		g9s##TYPE, g11s##TYPE, g13s##TYPE, g15s##TYPE);	\
  DEBUG_NL;							\
}

extern void abort (void);

X(ld, long double)

T(Sld1, long double)
T(Sld2, long double)
T(Sld3, long double)
T(Sld4, long double)
T(Sld5, long double)
T(Sld6, long double)
T(Sld7, long double)
T(Sld8, long double)
T(Sld9, long double)
T(Sld10, long double)
T(Sld11, long double)
T(Sld12, long double)
T(Sld13, long double)
T(Sld14, long double)
T(Sld15, long double)
T(Sld16, long double)

#undef T

void
struct_by_value_7_x ()
{
#define T(TYPE, MTYPE) testit##TYPE ();

T(Sld1, long double)
T(Sld2, long double)
T(Sld3, long double)
T(Sld4, long double)
T(Sld5, long double)
T(Sld6, long double)
T(Sld7, long double)
T(Sld8, long double)
T(Sld9, long double)
T(Sld10, long double)
T(Sld11, long double)
T(Sld12, long double)
T(Sld13, long double)
T(Sld14, long double)
T(Sld15, long double)
T(Sld16, long double)

#undef T
}
