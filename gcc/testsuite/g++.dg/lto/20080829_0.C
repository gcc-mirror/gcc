// { dg-lto-do assemble }

/* The replacement of cos+sin with __builtin_cexpi done by
   pass_cse_sincos was using a builtin for which we had no attributes.
   This was causing the operand scanner to materialize a VDEF at the
   builtin call-site which was not marked for renaming, thus tripping
   up the SSA verifier.  */
extern "C" { extern double cos (double); extern double sin (double); }
double func(double &in) { return cos(in) + sin(in); }
