/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/unidentified-key.json -fdump-tuning-model=temp.json" } */
/* { dg-additional-options "-fdiagnostics-show-caret -fdiagnostics-show-line-numbers" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */

/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/unidentified-key.json: In JSON value '/tune_params/unidentified_key'" } */
/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/unidentified-key.json:3:25: warning: key .* is not a tuning parameter, skipping"  "" { target *-*-* } 0 } */

/* { dg-begin-multiline-output "" }
    3 |     "unidentified_key": "10"
      |                         ^~~~
   { dg-end-multiline-output "" } */

int main () {}
