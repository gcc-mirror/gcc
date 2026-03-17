/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/integer-2.json -fdump-tuning-model=temp.json " } */
/* { dg-additional-options "-fdiagnostics-show-caret -fdiagnostics-show-line-numbers" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */

/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/integer-2.json: In JSON value '/tune_params/int_reassoc_width'" } */
/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/integer-2.json:3:26: error: key .* value .* is out of range for 'int' type \\\[.*, .*\\\]" } */
/* { dg-begin-multiline-output "" }
    3 |     "int_reassoc_width": 12097307449014
      |                          ^~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

/* { dg-error "validation failed for the provided JSON data"  "" { target *-*-* } 0 } */

int main () {}
