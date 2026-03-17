/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/unsigned-3.json -fdump-tuning-model=temp.json" } */
/* { dg-additional-options "-fdiagnostics-show-caret -fdiagnostics-show-line-numbers" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */

/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/unsigned-3.json: In JSON value '/tune_params/sve_width'" } */
/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/unsigned-3.json:3:18: error: key .* value .* is out of range for 'uint' type \\\[.*, .*\\\]" } */
/* { dg-begin-multiline-output "" }
    3 |     "sve_width": 5000000000
      |                  ^~~~~~~~~~
   { dg-end-multiline-output "" } */

/* { dg-error "validation failed for the provided JSON data"  "" { target *-*-* } 0 } */

int main () {}
