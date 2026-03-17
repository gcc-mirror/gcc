/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/integer-3.json -fdump-tuning-model=temp.json" } */
/* { dg-additional-options "-fdiagnostics-show-caret -fdiagnostics-show-line-numbers" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */

/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/integer-3.json: In JSON value '/tune_params/issue_rate'" } */
/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/integer-3.json:3:19: error: key .* expected to be an integer" } */
/* { dg-begin-multiline-output "" }
    3 |     "issue_rate": "10"
      |                   ^~~~
   { dg-end-multiline-output "" } */


/* { dg-error "validation failed for the provided JSON data"  "" { target *-*-* } 0 } */

int main () {}
