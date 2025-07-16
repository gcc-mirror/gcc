/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/enum-1.json -fdump-tuning-model=temp.json" } */

/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */
/* { dg-final { scan-file "temp.json" "\"autoprefetcher_model\": \"AUTOPREFETCHER_OFF\"" } } */
/* { dg-final { scan-file "temp.json" "\"ldp_policy_model\": \"AARCH64_LDP_STP_POLICY_NEVER\"" } } */
/* { dg-final { scan-file "temp.json" "\"stp_policy_model\": \"AARCH64_LDP_STP_POLICY_DEFAULT\"" } } */

int main () {}