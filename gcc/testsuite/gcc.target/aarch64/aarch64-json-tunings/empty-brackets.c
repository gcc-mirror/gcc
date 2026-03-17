/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/empty-brackets.json -fdump-tuning-model=temp.json" } */
/* { dg-additional-options "-fdiagnostics-show-caret -fdiagnostics-show-line-numbers" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */
/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/empty-brackets.json:1:1: warning: key 'tune_params' not found in JSON data" } */
/* { dg-begin-multiline-output "" }
    1 | {}
      | ^~
   { dg-end-multiline-output "" } */

int main () {}
