#include <stdarg.h>

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

/* Turn off checking for variable arguments with -DSKIPVA.  */
#ifdef SKIPVA
const int test_va = 0;
#else
const int test_va = 1;
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
void initS##NAME##1 (S##NAME##1 *p, TYPEM y)			\
{ p->a = y; }							\
void initS##NAME##2 (S##NAME##2 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; }					\
void initS##NAME##3 (S##NAME##3 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; }				\
void initS##NAME##4 (S##NAME##4 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; }		\
void initS##NAME##5 (S##NAME##5 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4; }	\
void initS##NAME##6 (S##NAME##6 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; }							\
void initS##NAME##7 (S##NAME##7 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; }					\
void initS##NAME##8 (S##NAME##8 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; }				\
void initS##NAME##9 (S##NAME##9 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; }		\
void initS##NAME##10 (S##NAME##10 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; p->j = y+9; }	\
void initS##NAME##11 (S##NAME##11 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; p->j = y+9;	\
  p->k = y+10; }						\
void initS##NAME##12 (S##NAME##12 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; p->j = y+9;	\
  p->k = y+10; p->l = y+11; }					\
void initS##NAME##13 (S##NAME##13 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; p->j = y+9;	\
  p->k = y+10; p->l = y+11; p->m = y+12; }			\
void initS##NAME##14 (S##NAME##14 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; p->j = y+9;	\
  p->k = y+10; p->l = y+11; p->m = y+12; p->n = y+13; }		\
void initS##NAME##15 (S##NAME##15 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; p->j = y+9;	\
  p->k = y+10; p->l = y+11; p->m = y+12; p->n = y+13;		\
  p->o = y+14; }						\
void initS##NAME##16 (S##NAME##16 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; p->j = y+9;	\
  p->k = y+10; p->l = y+11; p->m = y+12; p->n = y+13;		\
  p->o = y+14; p->p = y+15; }

#define T(TYPE,TYPE2)						\
extern TYPE g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE;		\
extern TYPE g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE;		\
extern TYPE g9s##TYPE, g10s##TYPE, g11s##TYPE, g12s##TYPE;	\
extern TYPE g13s##TYPE, g14s##TYPE, g15s##TYPE, g16s##TYPE;	\
								\
extern void check##TYPE (TYPE x, TYPE2 y);			\
								\
void								\
checkg##TYPE (void)						\
{								\
  check##TYPE (  g1s##TYPE,  (TYPE2)1);				\
  check##TYPE (  g2s##TYPE,  (TYPE2)2);				\
  check##TYPE (  g3s##TYPE,  (TYPE2)3);				\
  check##TYPE (  g4s##TYPE,  (TYPE2)4);				\
  check##TYPE (  g5s##TYPE,  (TYPE2)5);				\
  check##TYPE (  g6s##TYPE,  (TYPE2)6);				\
  check##TYPE (  g7s##TYPE,  (TYPE2)7);				\
  check##TYPE (  g8s##TYPE,  (TYPE2)8);				\
  check##TYPE (  g9s##TYPE,  (TYPE2)9);				\
  check##TYPE ( g10s##TYPE, (TYPE2)10);				\
  check##TYPE ( g11s##TYPE, (TYPE2)11);				\
  check##TYPE ( g12s##TYPE, (TYPE2)12);				\
  check##TYPE ( g13s##TYPE, (TYPE2)13);				\
  check##TYPE ( g14s##TYPE, (TYPE2)14);				\
  check##TYPE ( g15s##TYPE, (TYPE2)15);				\
  check##TYPE ( g16s##TYPE, (TYPE2)16);				\
}								\
								\
void								\
test##TYPE (TYPE s1, TYPE s2, TYPE s3, TYPE s4,			\
	    TYPE s5, TYPE s6, TYPE s7, TYPE s8,			\
	    TYPE s9, TYPE s10, TYPE s11, TYPE s12,		\
	    TYPE s13, TYPE s14, TYPE s15, TYPE s16)		\
{								\
  DEBUG_DOT;							\
  check##TYPE (s1, (TYPE2)1);					\
  DEBUG_DOT;							\
  check##TYPE (s2, (TYPE2)2);					\
  DEBUG_DOT;							\
  check##TYPE (s3, (TYPE2)3);					\
  DEBUG_DOT;							\
  check##TYPE (s4, (TYPE2)4);					\
  DEBUG_DOT;							\
  check##TYPE (s5, (TYPE2)5);					\
  DEBUG_DOT;							\
  check##TYPE (s6, (TYPE2)6);					\
  DEBUG_DOT;							\
  check##TYPE (s7, (TYPE2)7);					\
  DEBUG_DOT;							\
  check##TYPE (s8, (TYPE2)8);					\
  DEBUG_DOT;							\
  check##TYPE (s9, (TYPE2)9);					\
  DEBUG_DOT;							\
  check##TYPE (s10, (TYPE2)10);					\
  DEBUG_DOT;							\
  check##TYPE (s11, (TYPE2)11);					\
  DEBUG_DOT;							\
  check##TYPE (s12, (TYPE2)12);					\
  DEBUG_DOT;							\
  check##TYPE (s13, (TYPE2)13);					\
  DEBUG_DOT;							\
  check##TYPE (s14, (TYPE2)14);					\
  DEBUG_DOT;							\
  check##TYPE (s15, (TYPE2)15);					\
  DEBUG_DOT;							\
  check##TYPE (s16, (TYPE2)16);					\
}								\
								\
void								\
testva##TYPE (int n, ...)					\
{								\
  int i;							\
  va_list ap;							\
  if (test_va)							\
    {								\
      va_start (ap, n);						\
      for (i = 0; i < n; i++)					\
	{							\
	  TYPE t = va_arg (ap, TYPE);				\
	  DEBUG_DOT;						\
	  check##TYPE (t, (TYPE2)i+1);				\
	}							\
      va_end (ap);						\
    }								\
}

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
