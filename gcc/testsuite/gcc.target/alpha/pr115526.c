/* PR target/115526 */
/* { dg-do assemble } */
/* { dg-options "-Wno-attributes -fvisibility=hidden -fPIC -mcpu=ev4" } */

struct _ts {
  struct _dtoa_state *interp;
};
struct Bigint {
  int k;
} *_Py_dg_strtod_bs;
struct _dtoa_state {
  struct Bigint p5s;
  struct Bigint *freelist[];
};
extern _Thread_local struct _ts _Py_tss_tstate;
typedef struct Bigint Bigint;
int pow5mult_k;
long _Py_dg_strtod_ndigits;
void PyMem_Free();
void Bfree(Bigint *v) {
  if (v)
    {
      if (v->k)
	PyMem_Free();
      else {
	struct _dtoa_state *interp = _Py_tss_tstate.interp;
	interp->freelist[v->k] = v;
      }
    }
}
static Bigint *pow5mult(Bigint *b) {
  for (;;) {
    if (pow5mult_k & 1) {
      Bfree(b);
      if (b == 0)
        return 0;
    }
    if (!(pow5mult_k >>= 1))
      break;
  }
  return 0;
}
void _Py_dg_strtod() {
  if (_Py_dg_strtod_ndigits)
    pow5mult(_Py_dg_strtod_bs);
}
