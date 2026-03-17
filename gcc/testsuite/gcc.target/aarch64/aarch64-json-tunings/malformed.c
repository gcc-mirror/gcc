/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/malformed.json -fdump-tuning-model=temp.json" } */
/* { dg-additional-options "-fdiagnostics-show-caret -fdiagnostics-show-line-numbers" } */

/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/malformed.json:3:17: error: error parsing JSON data: expected ':'; got number" } */
/* { dg-begin-multiline-output "" }
    3 |     "sve_width" 128
      |                 ^~~
   { dg-end-multiline-output "" } */

int main () {}
