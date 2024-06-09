#include <stdarg.h>

typedef unsigned _BitInt(63) BI __attribute__((aligned(ALIGN)));

#ifndef EXTRA
#define EXTRA unsigned long long x;
#endif

struct S1  { __attribute__((aligned(1))) BI i : 1; EXTRA };
struct S8  { __attribute__((aligned(8))) BI i : 1; EXTRA };
struct S16 { __attribute__((aligned(16))) BI i : 1; EXTRA };

struct Sp   { BI i : 1; EXTRA }__attribute__((packed));
struct S1p  { __attribute__((packed, aligned(1))) BI i : 1; EXTRA };
struct S8p  { __attribute__((packed, aligned(8))) BI i : 1; EXTRA };
struct S16p { __attribute__((packed, aligned(16))) BI i : 1; EXTRA };

/* Bitfield in registers.  */
#define PARAMS(xx) int a0, struct S##xx s, BI a1
/* Bitfield passed by the stack.  */
#define PARAMS_STACK(xx) int a0, BI a1, BI a2, BI a3, BI a4, BI a5, BI a6, BI a7, BI a8, struct S##xx t
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

#define ARGS(xx) y, (struct S##xx) { x }, x
#define ARGS_STACK(xx) y, y, y, y, y, y, y, y, y, (struct S##xx) { x }
#define ARGS_STDARG(xx) y, (struct S##xx) { x }

/* Bitfield in registers.  */
_BitInt(63) __attribute__ ((noipa)) f1 (PARAMS(1))  { CODE(1) }
_BitInt(63) __attribute__ ((noipa)) f8 (PARAMS(8))  { CODE(8) }
_BitInt(63) __attribute__ ((noipa)) f16(PARAMS(16)) { CODE(16) }

_BitInt(63) __attribute__ ((noipa)) fp  (PARAMS(p))   { CODE(p) }
_BitInt(63) __attribute__ ((noipa)) f1p (PARAMS(1p))  { CODE(1p) }
_BitInt(63) __attribute__ ((noipa)) f8p (PARAMS(8p))  { CODE(8p) }
_BitInt(63) __attribute__ ((noipa)) f16p(PARAMS(16p)) { CODE(16p) }

_BitInt(63) g1 (_BitInt(63) x, int y) { return f1 (ARGS(1)); }
_BitInt(63) g8 (_BitInt(63) x, int y) { return f8 (ARGS(8)); }
_BitInt(63) g16(_BitInt(63) x, int y) { return f16 (ARGS(16)); }

_BitInt(63) gp  (_BitInt(63) x, int y) { return fp   (ARGS(p)); }
_BitInt(63) g1p (_BitInt(63) x, int y) { return f1p  (ARGS(1p)); }
_BitInt(63) g8p (_BitInt(63) x, int y) { return f8p  (ARGS(8p)); }
_BitInt(63) g16p(_BitInt(63) x, int y) { return f16p (ARGS(16p)); }

/* Bitfield in the stack.  */
_BitInt(63) __attribute__ ((noipa)) f1_stack (PARAMS_STACK(1))  { CODE_STACK(1) }
_BitInt(63) __attribute__ ((noipa)) f8_stack (PARAMS_STACK(8))  { CODE_STACK(8) }
_BitInt(63) __attribute__ ((noipa)) f16_stack(PARAMS_STACK(16)) { CODE_STACK(16) }

_BitInt(63) __attribute__ ((noipa)) fp_stack  (PARAMS_STACK(p))   { CODE_STACK(p) }
_BitInt(63) __attribute__ ((noipa)) f1p_stack (PARAMS_STACK(1p))  { CODE_STACK(1p) }
_BitInt(63) __attribute__ ((noipa)) f8p_stack (PARAMS_STACK(8p))  { CODE_STACK(8p) }
_BitInt(63) __attribute__ ((noipa)) f16p_stack(PARAMS_STACK(16p)) { CODE_STACK(16p) }


_BitInt(63) g1_stack (_BitInt(63) x, int y) { return f1_stack (ARGS_STACK(1)); }
_BitInt(63) g8_stack (_BitInt(63) x, int y) { return f8_stack (ARGS_STACK(8)); }
_BitInt(63) g16_stack(_BitInt(63) x, int y) { return f16_stack (ARGS_STACK(16)); }

_BitInt(63) gp_stack  (_BitInt(63) x, int y) { return fp_stack (ARGS_STACK(p)); }
_BitInt(63) g1p_stack (_BitInt(63) x, int y) { return f1p_stack (ARGS_STACK(1p)); }
_BitInt(63) g8p_stack (_BitInt(63) x, int y) { return f8p_stack (ARGS_STACK(8p)); }
_BitInt(63) g16p_stack(_BitInt(63) x, int y) { return f16p_stack (ARGS_STACK(16p)); }


/* Bitfield  via stdarg.  */
_BitInt(63) __attribute__ ((noipa)) f1_stdarg (PARAMS_STDARG(1))  { CODE_STDARG(1) }
_BitInt(63) __attribute__ ((noipa)) f8_stdarg (PARAMS_STDARG(8))  { CODE_STDARG(8) }
_BitInt(63) __attribute__ ((noipa)) f16_stdarg(PARAMS_STDARG(16)) { CODE_STDARG(16) }

_BitInt(63) __attribute__ ((noipa)) fp_stdarg  (PARAMS_STDARG(p))   { CODE_STDARG(p) }
_BitInt(63) __attribute__ ((noipa)) f1p_stdarg (PARAMS_STDARG(1p))  { CODE_STDARG(1p) }
_BitInt(63) __attribute__ ((noipa)) f8p_stdarg (PARAMS_STDARG(8p))  { CODE_STDARG(8p) }
_BitInt(63) __attribute__ ((noipa)) f16p_stdarg(PARAMS_STDARG(16p)) { CODE_STDARG(16p) }

_BitInt(63) g1_stdarg (_BitInt(63) x, int y) { return f1_stdarg (ARGS_STDARG(1)); }
_BitInt(63) g8_stdarg (_BitInt(63) x, int y) { return f8_stdarg (ARGS_STDARG(8)); }
_BitInt(63) g16_stdarg(_BitInt(63) x, int y) { return f16_stdarg (ARGS_STDARG(16)); }

_BitInt(63) gp_stdarg  (_BitInt(63) x, int y) { return fp_stdarg (ARGS_STDARG(p)); }
_BitInt(63) g1p_stdarg (_BitInt(63) x, int y) { return f1p_stdarg (ARGS_STDARG(1p)); }
_BitInt(63) g8p_stdarg (_BitInt(63) x, int y) { return f8p_stdarg (ARGS_STDARG(8p)); }
_BitInt(63) g16p_stdarg(_BitInt(63) x, int y) { return f16p_stdarg (ARGS_STDARG(16p)); }


