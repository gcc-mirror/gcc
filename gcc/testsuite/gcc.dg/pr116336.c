/* { dg-do compile } */
/* { dg-options "-O2 -gno-statement-frontiers -fcompare-debug" } */

double duk_js_execute_bytecode_duk__tv_0_0;
double duk_double_div();
void duk_handle_call_unprotected();
void duk_js_execute_bytecode() {
  double du_0;
  long opcode_shifted;
  switch (opcode_shifted)
  case 2:
    du_0 = duk_double_div();
  duk_js_execute_bytecode_duk__tv_0_0 = du_0;
  duk_handle_call_unprotected();
  duk_js_execute_bytecode();
}
