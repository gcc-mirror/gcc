/* This tests returning of structures.  */

#include <stdio.h>
#include "bf16-check.h"
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

#define xmm0b xmm_regs[0].___bf16
#define xmm1b xmm_regs[1].___bf16
#define xmm0f xmm_regs[0]._float
#define xmm0d xmm_regs[0]._double
#define xmm1f xmm_regs[1]._float
#define xmm1d xmm_regs[1]._double

typedef enum {
  SSE_B = 0,
  SSE_D,
  MEM,
  INT_SSE,
  SSE_INT,
  SSE_F_H,
  SSE_F_H8
} Type;

/* Structures which should be returned in SSE.  */
#define D(I,MEMBERS,C,B) struct S_ ## I { MEMBERS ; }; Type class_ ## I = C; \
struct S_ ## I f_ ## I (void) { struct S_ ## I s; memset (&s, 0, sizeof(s)); B; return s; }

D(120,__bf16 f,SSE_B, s.f=make_f32_bf16(42.0f))
D(121,__bf16 f;__bf16 f2,SSE_B, s.f=make_f32_bf16(42.0f))
D(122,__bf16 f;float d,SSE_B, s.f=make_f32_bf16(42.0f))
D(123,__bf16 f;double d,SSE_B, s.f=make_f32_bf16(42.0f))
D(124,double d; __bf16 f,SSE_D, s.d=42)
D(125,__bf16 f[2],SSE_B, s.f[0]=make_f32_bf16(42.0f))
D(126,__bf16 f[3],SSE_B, s.f[0]=make_f32_bf16(42.0f))
D(127,__bf16 f[4],SSE_B, s.f[0]=make_f32_bf16(42.0f))
D(128,__bf16 f[2]; double d,SSE_B, s.f[0]=make_f32_bf16(42.0f))
D(129,double d;__bf16 f[2],SSE_D, s.d=42)

#undef D

#define D(I,MEMBERS) struct S_ ## I { MEMBERS ; }; Type class_ ## I = INT_SSE; \
struct S_ ## I f_ ## I (void) { struct S_ ## I s = { 42, make_f32_bf16(43.0f) }; return s; }

D(310,char m1; __bf16 m2)
D(311,short m1; __bf16 m2)
D(312,int m1; __bf16 m2)
D(313,long long m1; __bf16 m2)

#undef D

void check_300 (void)
{
  XMM_T x;
  x._ulonglong[0] = rax;
  switch (current_test) {
    case 310: assert ((rax & 0xff) == 42
		      && check_bf16_float (x.___bf16[1], 43.0f) == 1); break;
    case 311: assert ((rax & 0xffff) == 42
		      && check_bf16_float (x.___bf16[1], 43.0f) == 1); break;
    case 312: assert ((rax & 0xffffffff) == 42
		      && check_bf16_float (x.___bf16[2], 43.0f) == 1); break;
    case 313: assert (rax == 42
		      && check_bf16_float (xmm0b[0], 43.0f) == 1); break;

    default: assert (0); break;
  }
}

/* Structures which should be returned in SSE (low) and INT (high).  */
#define D(I,MEMBERS,B) struct S_ ## I { MEMBERS ; }; Type class_ ## I = SSE_INT; \
struct S_ ## I f_ ## I (void) { struct S_ ## I s; memset (&s, 0, sizeof(s));  B; return s; }

D(402,__bf16 f[4];char c, s.f[0]=make_f32_bf16(42.0f); s.c=43)

#undef D

void check_400 (void)
{
  switch (current_test) {
    case 402: assert (check_bf16_float (xmm0b[0], 42.0f) == 1 && (rax & 0xff) == 43); break;

    default: assert (0); break;
  }
}

/* Structures which should be returned in MEM.  */
void *struct_addr;
#define D(I,MEMBERS) struct S_ ## I { MEMBERS ; }; Type class_ ## I = MEM; \
struct S_ ## I f_ ## I (void) { union {unsigned char c; struct S_ ## I s;} u; memset (&u.s, 0, sizeof(u.s)); u.c = 42; return u.s; }

/* Unnaturally aligned members.  */
D(540,__bf16 m1[10])
D(541,char m1[1];__bf16 f[8])

#undef D


/* Special tests.  */
#define D(I,MEMBERS,C,B) struct S_ ## I { MEMBERS ; }; Type class_ ## I = C; \
struct S_ ## I f_ ## I (void) { struct S_ ## I s; B; return s; }
D(601,__bf16 f[4], SSE_F_H, s.f[0] = s.f[1] = s.f[2] = s.f[3] = make_f32_bf16 (42.0f))
D(602,__bf16 f[8], SSE_F_H8,
  s.f[0] = s.f[1] = s.f[2] = s.f[3] = s.f[4] = s.f[5] = s.f[6] = s.f[7] = make_f32_bf16 (42.0f))
#undef D

void clear_all (void)
{
  clear_int_registers;
}

void check_all (Type class, unsigned long size)
{
  switch (class) {
    case SSE_B: assert (check_bf16_float (xmm0b[0], 42.0f) == 1); break;
    case SSE_D: assert (xmm0d[0] == 42); break;
    case SSE_F_H: assert (check_bf16_float (xmm0b[0], 42) == 1
			  && check_bf16_float (xmm0b[1], 42) == 1
			  && check_bf16_float (xmm0b[2], 42) == 1
			  && check_bf16_float (xmm0b[3], 42) == 1); break;
    case SSE_F_H8: assert (check_bf16_float (xmm0b[0], 42) == 1
			   && check_bf16_float (xmm0b[1], 42) == 1
			   && check_bf16_float (xmm0b[2], 42) == 1
			   && check_bf16_float (xmm0b[3], 42) == 1
			   && check_bf16_float (xmm1b[0], 42) == 1
			   && check_bf16_float (xmm1b[1], 42) == 1
			   && check_bf16_float (xmm1b[2], 42) == 1
			   && check_bf16_float (xmm1b[3], 42) == 1); break;
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
  D(120) D(121) D(122) D(123) D(124) D(125) D(126) D(127) D(128) D(129)

  D(310) D(311) D(312) D(313)

  D(402)

  D(540) D(541)

  D(601) D(602)
  if (num_failed)
    abort ();
}
#undef D
