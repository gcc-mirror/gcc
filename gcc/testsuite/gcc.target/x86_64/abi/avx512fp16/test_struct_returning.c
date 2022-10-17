/* This tests returning of structures.  */

#include <stdio.h>
#include "avx512fp16-xmm-check.h"
#include "defines.h"
#include "macros.h"
#include "args.h"

struct IntegerRegisters iregs;
struct FloatRegisters fregs;
unsigned int num_iregs, num_fregs;

int current_test;
int num_failed = 0;

#undef assert
#define assert(test) do { if (!(test)) {fprintf (stderr, "failed in test %d\n", current_test); num_failed++; } } while (0)

#define xmm0h xmm_regs[0].__Float16
#define xmm1h xmm_regs[1].__Float16
#define xmm0f xmm_regs[0]._float
#define xmm0d xmm_regs[0]._double
#define xmm1f xmm_regs[1]._float
#define xmm1d xmm_regs[1]._double

typedef enum {
  INT = 0,
  SSE_H,
  SSE_F,
  SSE_D,
  X87,
  MEM,
  INT_SSE,
  SSE_INT,
  SSE_F_V,
  SSE_F_H,
  SSE_F_H8
} Type;

/* Structures which should be returned in INTEGER.  */
#define D(I,MEMBERS,B) struct S_ ## I { MEMBERS ; }; Type class_ ## I = INT; \
struct S_ ## I f_ ## I (void) { struct S_ ## I s; memset (&s, 0, sizeof(s)); B; return s; }

D(1,char m1, s.m1=42)
D(2,short m1, s.m1=42)
D(3,int m1, s.m1=42)
D(4,long m1, s.m1=42)
D(5,long long m1, s.m1=42)
D(6,char m1;short s, s.m1=42)
D(7,char m1;int i, s.m1=42)
D(8,char m1; long l, s.m1=42)
D(9,char m1; long long l, s.m1=42)
D(10,char m1[16], s.m1[0]=42)
D(11,short m1[8], s.m1[0]=42)
D(12,int m1[4], s.m1[0]=42)
D(13,long m1[2], s.m1[0]=42)
D(14,long long m1[2], s.m1[0]=42)

#undef D

/* Structures which should be returned in SSE.  */
#define D(I,MEMBERS,C,B) struct S_ ## I { MEMBERS ; }; Type class_ ## I = C; \
struct S_ ## I f_ ## I (void) { struct S_ ## I s; memset (&s, 0, sizeof(s)); B; return s; }

D(100,float f,SSE_F, s.f=42)
D(101,double d,SSE_D, s.d=42)
D(102,float f;float f2,SSE_F, s.f=42)
D(103,float f;double d,SSE_F, s.f=42)
D(104,double d; float f,SSE_D, s.d=42)
D(105,double d; double d2,SSE_D, s.d=42)
D(106,float f[2],SSE_F, s.f[0]=42)
D(107,float f[3],SSE_F, s.f[0]=42)
D(108,float f[4],SSE_F, s.f[0]=42)
D(109,double d[2],SSE_D, s.d[0]=42)
D(110,float f[2]; double d,SSE_F, s.f[0]=42)
D(111,double d;float f[2],SSE_D, s.d=42)

D(120,_Float16 f,SSE_H, s.f=42)
D(121,_Float16 f;_Float16 f2,SSE_H, s.f=42)
D(122,_Float16 f;float d,SSE_H, s.f=42)
D(123,_Float16 f;double d,SSE_H, s.f=42)
D(124,double d; _Float16 f,SSE_D, s.d=42)
D(125,_Float16 f[2],SSE_H, s.f[0]=42)
D(126,_Float16 f[3],SSE_H, s.f[0]=42)
D(127,_Float16 f[4],SSE_H, s.f[0]=42)
D(128,_Float16 f[2]; double d,SSE_H, s.f[0]=42)
D(129,double d;_Float16 f[2],SSE_D, s.d=42)

#undef D

/* Structures which should be returned on x87 stack.  */
#define D(I,MEMBERS) struct S_ ## I { MEMBERS ; }; Type class_ ## I = X87; \
struct S_ ## I f_ ## I (void) { struct S_ ## I s = { 42 }; return s; }

/* The only struct containing a long double, which is returned in
   registers at all, is the singleton struct.  All others are too large.
   This includes a struct containing complex long double, which is passed
   in memory, although a complex long double type itself is returned in
   two registers.  */
D(200,long double ld)

#undef D

/* Structures which should be returned in INT (low) and SSE (high).  */
#define D(I,MEMBERS) struct S_ ## I { MEMBERS ; }; Type class_ ## I = INT_SSE; \
struct S_ ## I f_ ## I (void) { struct S_ ## I s = { 42,43 }; return s; }

D(300,char m1; float m2)
D(301,char m1; double m2)
D(302,short m1; float m2)
D(303,short m1; double m2)
D(304,int m1; float m2)
D(305,int m1; double m2)
D(306,long long m1; float m2)
D(307,long long m1; double m2)

D(310,char m1; _Float16 m2)
D(311,short m1; _Float16 m2)
D(312,int m1; _Float16 m2)
D(313,long long m1; _Float16 m2)

#undef D

void check_300 (void)
{
  XMM_T x;
  x._ulonglong[0] = rax;
  switch (current_test) {
    case 300: assert ((rax & 0xff) == 42 && x._float[1] == 43); break;
    case 301: assert ((rax & 0xff) == 42 && xmm0d[0] == 43); break;
    case 302: assert ((rax & 0xffff) == 42 && x._float[1] == 43); break;
    case 303: assert ((rax & 0xffff) == 42 && xmm0d[0] == 43); break;
    case 304: assert ((rax & 0xffffffff) == 42 && x._float[1] == 43); break;
    case 305: assert ((rax & 0xffffffff) == 42 && xmm0d[0] == 43); break;
    case 306: assert (rax == 42 && xmm0f[0] == 43); break;
    case 307: assert (rax == 42 && xmm0d[0] == 43); break;
    case 310: assert ((rax & 0xff) == 42 && x.__Float16[1] == 43); break;
    case 311: assert ((rax & 0xffff) == 42 && x.__Float16[1] == 43); break;
    case 312: assert ((rax & 0xffffffff) == 42 && x.__Float16[2] == 43); break;
    case 313: assert (rax == 42 && xmm0h[0] == 43); break;

    default: assert (0); break;
  }
}

/* Structures which should be returned in SSE (low) and INT (high).  */
#define D(I,MEMBERS,B) struct S_ ## I { MEMBERS ; }; Type class_ ## I = SSE_INT; \
struct S_ ## I f_ ## I (void) { struct S_ ## I s; memset (&s, 0, sizeof(s));  B; return s; }

D(400,float f[2];char c, s.f[0]=42; s.c=43)
D(401,double d;char c, s.d=42; s.c=43)

D(402,_Float16 f[4];char c, s.f[0]=42; s.c=43)

#undef D

void check_400 (void)
{
  switch (current_test) {
    case 400: assert (xmm0f[0] == 42 && (rax & 0xff) == 43); break;
    case 401: assert (xmm0d[0] == 42 && (rax & 0xff) == 43); break;
    case 402: assert (xmm0h[0] == 42 && (rax & 0xff) == 43); break;

    default: assert (0); break;
  }
}

/* Structures which should be returned in MEM.  */
void *struct_addr;
#define D(I,MEMBERS) struct S_ ## I { MEMBERS ; }; Type class_ ## I = MEM; \
struct S_ ## I f_ ## I (void) { union {unsigned char c; struct S_ ## I s;} u; memset (&u.s, 0, sizeof(u.s)); u.c = 42; return u.s; }

/* Too large.  */
D(500,char m1[17])
D(501,short m1[9])
D(502,int m1[5])
D(503,long m1[3])
D(504,short m1[8];char c)
D(505,char m1[1];int i[4])
D(506,float m1[5])
D(507,double m1[3])
D(508,char m1[1];float f[4])
D(509,char m1[1];double d[2])
D(510,__complex long double m1[1])

/* Too large due to padding.  */
D(520,char m1[1];int i;char c2; int i2; char c3)

/* Unnaturally aligned members.  */
D(530,short m1[1];int i PACKED)

D(540,_Float16 m1[10])
D(541,char m1[1];_Float16 f[8])

#undef D


/* Special tests.  */
#define D(I,MEMBERS,C,B) struct S_ ## I { MEMBERS ; }; Type class_ ## I = C; \
struct S_ ## I f_ ## I (void) { struct S_ ## I s; B; return s; }
D(600,float f[4], SSE_F_V, s.f[0] = s.f[1] = s.f[2] = s.f[3] = 42)
D(601,_Float16 f[4], SSE_F_H, s.f[0] = s.f[1] = s.f[2] = s.f[3] = 42)
D(602,_Float16 f[8], SSE_F_H8,
  s.f[0] = s.f[1] = s.f[2] = s.f[3] = s.f[4] = s.f[5] = s.f[6] = s.f[7] = 42)
#undef D

void clear_all (void)
{
  clear_int_registers;
  clear_float_registers;
  clear_x87_registers;
}

void check_all (Type class, unsigned long size)
{
  switch (class) {
    case INT: if (size < 8) rax &= ~0UL >> (64-8*size); assert (rax == 42); break;
    case SSE_H: assert (xmm0h[0] == 42); break;
    case SSE_F: assert (xmm0f[0] == 42); break;
    case SSE_D: assert (xmm0d[0] == 42); break;
    case SSE_F_V: assert (xmm0f[0] == 42 && xmm0f[1]==42 && xmm1f[0] == 42 && xmm1f[1] == 42); break;
    case SSE_F_H: assert (xmm0h[0] == 42 && xmm0h[1]==42 && xmm0h[2] == 42 && xmm0h[3] == 42); break;
    case SSE_F_H8: assert (xmm0h[0] == 42 && xmm0h[1]==42 && xmm0h[2] == 42 && xmm0h[3] == 42
			   && xmm1h[0] == 42 && xmm1h[1]==42 && xmm1h[2] == 42 && xmm1h[3] == 42); break;
    case X87: assert (x87_regs[0]._ldouble == 42); break;
    case INT_SSE: check_300(); break;
    case SSE_INT: check_400(); break;
    /* Ideally we would like to check that rax == struct_addr.
       Unfortunately the address of the target struct escapes (for setting
       struct_addr), so the return struct is a temporary one whose address
       is given to the f_* functions, otherwise a conforming program
       could notice the struct changing already before the function returns.
       This temporary struct could be anywhere.  For GCC it will be on
       stack, but no one is forbidding that it could be a static variable
       if there's no threading or proper locking.  Nobody in his right mind
       will not use the stack for that.  */
    case MEM: assert (*(unsigned char*)struct_addr == 42 && rdi == rax); break;
  }
}

#define D(I) { struct S_ ## I s; current_test = I; struct_addr = (void*)&s; \
  clear_all(); \
  s = WRAP_RET(f_ ## I) (); \
  check_all(class_ ## I, sizeof(s)); \
}

static void
do_test (void)
{
  D(1) D(2) D(3) D(4) D(5) D(6) D(7) D(8) D(9) D(10) D(11) D(12) D(13) D(14)
  
  D(100) D(101) D(102) D(103) D(104) D(105) D(106) D(107) D(108) D(109) D(110)
  D(111)
  
  D(120) D(121) D(122) D(123) D(124) D(125) D(126) D(127) D(128) D(129)

  D(200)

  D(300) D(301) D(302) D(303) D(304) D(305) D(306) D(307)
  D(310) D(311) D(312) D(313)

  D(400) D(401) D(402)

  D(500) D(501) D(502) D(503) D(504) D(505) D(506) D(507) D(508) D(509)
  D(520)
  D(530)

  D(540) D(541)

  D(600) D(601) D(602)
  if (num_failed)
    abort ();
}
#undef D
