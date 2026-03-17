/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/string-2.json -fdump-tuning-model=temp.json" } */
/* { dg-additional-options "-fdiagnostics-show-caret -fdiagnostics-show-line-numbers" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */

/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/string-2.json: In JSON value '/tune_params/function_align'" } */
/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/string-2.json:3:23: error: key .* expected to be a string"  "" { target *-*-* } 0 } */
/* { dg-begin-multiline-output "" }
    3 |     "function_align": 16
      |                       ^~
   { dg-end-multiline-output "" } */

/* { dg-error "validation failed for the provided JSON data"  "" { target *-*-* } 0 } */

int main () {}
