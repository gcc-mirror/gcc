/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/enum-2.json -fdump-tuning-model=temp.json" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */
/* { dg-warning "autoprefetcher_model not recognized, defaulting to 'AUTOPREFETCHER_OFF'"  "" { target *-*-* } 0 } */
/* { dg-warning "ldp_policy_model not recognized, defaulting to 'AARCH64_LDP_STP_POLICY_DEFAULT'"  "" { target *-*-* } 0 } */

int main () {}