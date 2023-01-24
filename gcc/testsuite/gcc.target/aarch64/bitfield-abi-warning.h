#include <stdarg.h>

typedef unsigned long long ull __attribute__((aligned(ALIGN)));

#ifndef EXTRA
#define EXTRA unsigned long long x;
#endif

struct S1  { __attribute__((aligned(1))) ull i : 1; EXTRA };
struct S2  { __attribute__((aligned(2))) ull i : 1; EXTRA };
struct S4  { __attribute__((aligned(4))) ull i : 1; EXTRA };
struct S8  { __attribute__((aligned(8))) ull i : 1; EXTRA };
struct S16 { __attribute__((aligned(16))) ull i : 1; EXTRA };

struct Sp   { ull i : 1; EXTRA }__attribute__((packed));
struct S1p  { __attribute__((packed, aligned(1))) ull i : 1; EXTRA };
struct S2p  { __attribute__((packed, aligned(2))) ull i : 1; EXTRA };
struct S4p  { __attribute__((packed, aligned(4))) ull i : 1; EXTRA };
struct S8p  { __attribute__((packed, aligned(8))) ull i : 1; EXTRA };
struct S16p { __attribute__((packed, aligned(16))) ull i : 1; EXTRA };

/* Bitfield in registers.  */
#define PARAMS(xx) int a0, struct S##xx s, ull a1
/* Bitfield passed by the stack.  */
#define PARAMS_STACK(xx) int a0, ull a1, ull a2, ull a3, ull a4, ull a5, ull a6, ull a7, ull a8, struct S##xx t
/* Bitfield passed via stdarg.  */
#define PARAMS_STDARG(xx) int a0, ...

#define CODE(xx)				\
  return s.i;

#define CODE_STACK(xx)				\
  return t.i;

#define CODE_STDARG(xx)				\
  va_list ap;					\
  struct S##xx arg;				\
  __builtin_va_start(ap,a0);			\
  arg = __builtin_va_arg(ap, struct S##xx);	\
  return arg.i;

#define ARGS(xx) x, (struct S##xx) { x }, x
#define ARGS_STACK(xx) x, x, x, x, x, x, x, x, x, (struct S##xx) { x }
#define ARGS_STDARG(xx) x, (struct S##xx) { x }

/* Bitfield in registers.  */
int __attribute__ ((noipa)) f1 (PARAMS(1))  { CODE(1) }
int __attribute__ ((noipa)) f2 (PARAMS(2))  { CODE(2) }
int __attribute__ ((noipa)) f4 (PARAMS(4))  { CODE(4) }
int __attribute__ ((noipa)) f8 (PARAMS(8))  { CODE(8) }
int __attribute__ ((noipa)) f16(PARAMS(16)) { CODE(16) }

int __attribute__ ((noipa)) fp  (PARAMS(p))   { CODE(p) }
int __attribute__ ((noipa)) f1p (PARAMS(1p))  { CODE(1p) }
int __attribute__ ((noipa)) f2p (PARAMS(2p))  { CODE(2p) }
int __attribute__ ((noipa)) f4p (PARAMS(4p))  { CODE(4p) }
int __attribute__ ((noipa)) f8p (PARAMS(8p))  { CODE(8p) }
int __attribute__ ((noipa)) f16p(PARAMS(16p)) { CODE(16p) }

int g1 (int x) { return f1 (ARGS(1)); }
int g2 (int x) { return f2 (ARGS(2)); }
int g4 (int x) { return f4 (ARGS(4)); }
int g8 (int x) { return f8 (ARGS(8)); }
int g16(int x) { return f16 (ARGS(16)); }

int gp  (int x) { return fp   (ARGS(p)); }
int g1p (int x) { return f1p  (ARGS(1p)); }
int g2p (int x) { return f2p  (ARGS(2p)); }
int g4p (int x) { return f4p  (ARGS(4p)); }
int g8p (int x) { return f8p  (ARGS(8p)); }
int g16p(int x) { return f16p (ARGS(16p)); }

/* Bitfield in the stack.  */
int __attribute__ ((noipa)) f1_stack (PARAMS_STACK(1))  { CODE_STACK(1) }
int __attribute__ ((noipa)) f2_stack (PARAMS_STACK(2))  { CODE_STACK(2) }
int __attribute__ ((noipa)) f4_stack (PARAMS_STACK(4))  { CODE_STACK(4) }
int __attribute__ ((noipa)) f8_stack (PARAMS_STACK(8))  { CODE_STACK(8) }
int __attribute__ ((noipa)) f16_stack(PARAMS_STACK(16)) { CODE_STACK(16) }

int __attribute__ ((noipa)) fp_stack  (PARAMS_STACK(p))   { CODE_STACK(p) }
int __attribute__ ((noipa)) f1p_stack (PARAMS_STACK(1p))  { CODE_STACK(1p) }
int __attribute__ ((noipa)) f2p_stack (PARAMS_STACK(2p))  { CODE_STACK(2p) }
int __attribute__ ((noipa)) f4p_stack (PARAMS_STACK(4p))  { CODE_STACK(4p) }
int __attribute__ ((noipa)) f8p_stack (PARAMS_STACK(8p))  { CODE_STACK(8p) }
int __attribute__ ((noipa)) f16p_stack(PARAMS_STACK(16p)) { CODE_STACK(16p) }

int g1_stack (int x) { return f1_stack (ARGS_STACK(1)); }
int g2_stack (int x) { return f2_stack (ARGS_STACK(2)); }
int g4_stack (int x) { return f4_stack (ARGS_STACK(4)); }
int g8_stack (int x) { return f8_stack (ARGS_STACK(8)); }
int g16_stack(int x) { return f16_stack (ARGS_STACK(16)); }

int gp_stack  (int x) { return fp_stack (ARGS_STACK(p)); }
int g1p_stack (int x) { return f1p_stack (ARGS_STACK(1p)); }
int g2p_stack (int x) { return f2p_stack (ARGS_STACK(2p)); }
int g4p_stack (int x) { return f4p_stack (ARGS_STACK(4p)); }
int g8p_stack (int x) { return f8p_stack (ARGS_STACK(8p)); }
int g16p_stack(int x) { return f16p_stack (ARGS_STACK(16p)); }

/* Bitfield  via stdarg.  */
int __attribute__ ((noipa)) f1_stdarg (PARAMS_STDARG(1))  { CODE_STDARG(1) }
int __attribute__ ((noipa)) f2_stdarg (PARAMS_STDARG(2))  { CODE_STDARG(2) }
int __attribute__ ((noipa)) f4_stdarg (PARAMS_STDARG(4))  { CODE_STDARG(4) }
int __attribute__ ((noipa)) f8_stdarg (PARAMS_STDARG(8))  { CODE_STDARG(8) }
int __attribute__ ((noipa)) f16_stdarg(PARAMS_STDARG(16)) { CODE_STDARG(16) }

int __attribute__ ((noipa)) fp_stdarg  (PARAMS_STDARG(p))   { CODE_STDARG(p) }
int __attribute__ ((noipa)) f1p_stdarg (PARAMS_STDARG(1p))  { CODE_STDARG(1p) }
int __attribute__ ((noipa)) f2p_stdarg (PARAMS_STDARG(2p))  { CODE_STDARG(2p) }
int __attribute__ ((noipa)) f4p_stdarg (PARAMS_STDARG(4p))  { CODE_STDARG(4p) }
int __attribute__ ((noipa)) f8p_stdarg (PARAMS_STDARG(8p))  { CODE_STDARG(8p) }
int __attribute__ ((noipa)) f16p_stdarg(PARAMS_STDARG(16p)) { CODE_STDARG(16p) }

int g1_stdarg (int x) { return f1_stdarg (ARGS_STDARG(1)); }
int g2_stdarg (int x) { return f2_stdarg (ARGS_STDARG(2)); }
int g4_stdarg (int x) { return f4_stdarg (ARGS_STDARG(4)); }
int g8_stdarg (int x) { return f8_stdarg (ARGS_STDARG(8)); }
int g16_stdarg(int x) { return f16_stdarg (ARGS_STDARG(16)); }

int gp_stdarg  (int x) { return fp_stdarg (ARGS_STDARG(p)); }
int g1p_stdarg (int x) { return f1p_stdarg (ARGS_STDARG(1p)); }
int g2p_stdarg (int x) { return f2p_stdarg (ARGS_STDARG(2p)); }
int g4p_stdarg (int x) { return f4p_stdarg (ARGS_STDARG(4p)); }
int g8p_stdarg (int x) { return f8p_stdarg (ARGS_STDARG(8p)); }
int g16p_stdarg(int x) { return f16p_stdarg (ARGS_STDARG(16p)); }
