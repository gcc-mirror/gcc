/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/enum-2.json -fdump-tuning-model=temp.json" } */
/* { dg-additional-options "-fdiagnostics-show-caret -fdiagnostics-show-line-numbers" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */

/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/enum-2.json: In JSON value '/tune_params/autoprefetcher_model'" } */
/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/enum-2.json:3:29: warning: autoprefetcher_model not recognized, defaulting to 'AUTOPREFETCHER_OFF'" } */
/* { dg-begin-multiline-output "" }
    3 |     "autoprefetcher_model": "null",
      |                             ^~~~~~
   { dg-end-multiline-output "" } */

/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/enum-2.json: In JSON value '/tune_params/ldp_policy_model'" } */
/* { dg-regexp ".*gcc.target/aarch64/aarch64-json-tunings/enum-2.json:4:25: warning: ldp_policy_model not recognized, defaulting to 'AARCH64_LDP_STP_POLICY_DEFAULT'" } */
/* { dg-begin-multiline-output "" }
    4 |     "ldp_policy_model": "null",
      |                         ^~~~~~
   { dg-end-multiline-output "" } */

int main () {}
