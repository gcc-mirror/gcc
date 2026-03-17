/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/boolean-2.json -fdump-tuning-model=temp.json" } */
/* { dg-additional-options "-fdiagnostics-show-caret -fdiagnostics-show-line-numbers" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */

/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/boolean-2.json: In JSON value '/tune_params/insn_extra_cost/alu/non_exec_costs_exec'" } */
/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/boolean-2.json:5:32: error: key .* expected to be a boolean \\\(true/false\\\)"  } */
/* { dg-begin-multiline-output "" }
    5 |         "non_exec_costs_exec": 0
      |                                ^
   { dg-end-multiline-output "" } */

/* { dg-error "validation failed for the provided JSON data"  "" { target *-*-* } 0 } */

int main () {}
