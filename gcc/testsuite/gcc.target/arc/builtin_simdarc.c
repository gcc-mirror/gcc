/* { dg-do compile } */
/* { dg-options "-mcpu=archs -O2 -Werror-implicit-function-declaration -mmpy-option=9" } */

#define STEST(name, rettype, op1type, op2type)	\
  rettype test_ ## name				\
  (op1type a, op2type b)			\
  {						\
    return __builtin_arc_ ## name (a, b);	\
  }

typedef short v2hi __attribute__ ((vector_size (4)));
typedef short v4hi __attribute__ ((vector_size (8)));
typedef int   v2si __attribute__ ((vector_size (8)));

STEST (qmach,  long long, v4hi, v4hi)
STEST (qmachu, long long, v4hi, v4hi)
STEST (qmpyh,  long long, v4hi, v4hi)
STEST (qmpyhu, long long, v4hi, v4hi)

STEST (dmach,  int, v2hi, v2hi)
STEST (dmachu, int, v2hi, v2hi)
STEST (dmpyh,  int, v2hi, v2hi)
STEST (dmpyhu, int, v2hi, v2hi)

STEST (dmacwh,  long, v2si, v2hi)
STEST (dmacwhu, long, v2si, v2hi)

STEST (vmac2h,  v2si, v2hi, v2hi)
STEST (vmac2hu, v2si, v2hi, v2hi)
STEST (vmpy2h,  v2si, v2hi, v2hi)
STEST (vmpy2hu, v2si, v2hi, v2hi)

STEST (vaddsub2h, v2hi, v2hi, v2hi)
STEST (vsubadd2h, v2hi, v2hi, v2hi)
STEST (vaddsub,   v2si, v2si, v2si)
STEST (vsubadd,   v2si, v2si, v2si)
STEST (vaddsub4h, v4hi, v4hi, v4hi)
STEST (vsubadd4h, v4hi, v4hi, v4hi)
