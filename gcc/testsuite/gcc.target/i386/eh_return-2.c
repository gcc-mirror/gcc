/* PR target/101772  */
/* { dg-do compile } */
/* { dg-additional-options "-O0 -mincoming-stack-boundary=4 -march=x86-64 -mstackrealign" } */

struct _Unwind_Context _Unwind_Resume_or_Rethrow_this_context;

void offset (int);

struct _Unwind_Context {
  void *reg[7];
} _Unwind_Resume_or_Rethrow() {
  struct _Unwind_Context cur_contextcur_context =
      _Unwind_Resume_or_Rethrow_this_context;
  offset(0);
  __builtin_eh_return ((long) offset, 0);
}
