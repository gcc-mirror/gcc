/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/test-all.json -fdump-tuning-model=temp.json" } */
/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */

/* Test round-trip parsing: load JSON, dump it, verify key values are preserved */

/* Check basic structure values */
/* { dg-final { scan-file "temp.json" "\"issue_rate\": 3" } } */
/* { dg-final { scan-file "temp.json" "\"fusible_ops\": 48" } } */
/* { dg-final { scan-file "temp.json" "\"function_align\": \"32:16\"" } } */

/* Check alu costs */
/* { dg-final { scan-file "temp.json" "\"arith\": 0" } } */
/* { dg-final { scan-file "temp.json" "\"logical\": 0" } } */
/* { dg-final { scan-file "temp.json" "\"shift\": 0" } } */
/* { dg-final { scan-file "temp.json" "\"arith_shift\": 4" } } */

/* Check load/store costs */
/* { dg-final { scan-file "temp.json" "\"load\": 12" } } */
/* { dg-final { scan-file "temp.json" "\"store\": 0" } } */
/* { dg-final { scan-file "temp.json" "\"loadf\": 16" } } */
/* { dg-final { scan-file "temp.json" "\"storef\": 0" } } */

/* Check regmove costs */
/* { dg-final { scan-file "temp.json" "\"GP2GP\": 1" } } */
/* { dg-final { scan-file "temp.json" "\"GP2FP\": 5" } } */
/* { dg-final { scan-file "temp.json" "\"FP2GP\": 5" } } */
/* { dg-final { scan-file "temp.json" "\"FP2FP\": 2" } } */

/* Check vec_costs scalar fields */
/* { dg-final { scan-file "temp.json" "\"scalar_int_stmt_cost\": 1" } } */
/* { dg-final { scan-file "temp.json" "\"scalar_fp_stmt_cost\": 1" } } */
/* { dg-final { scan-file "temp.json" "\"cond_taken_branch_cost\": 3" } } */

/* Check vec_costs advsimd nested fields */
/* { dg-final { scan-file "temp.json" "\"int_stmt_cost\": 1" } } */
/* { dg-final { scan-file "temp.json" "\"fp_stmt_cost\": 1" } } */
/* { dg-final { scan-file "temp.json" "\"permute_cost\": 2" } } */
/* { dg-final { scan-file "temp.json" "\"vec_to_scalar_cost\": 2" } } */

/* Check vec_costs sve nested fields */
/* { dg-final { scan-file "temp.json" "\"clast_cost\": 2" } } */
/* { dg-final { scan-file "temp.json" "\"fadda_f32_cost\": 2" } } */
/* { dg-final { scan-file "temp.json" "\"gather_load_x32_cost\": 4" } } */

/* Check enum values */
/* { dg-final { scan-file "temp.json" "\"autoprefetcher_model\": \"AUTOPREFETCHER_WEAK\"" } } */
/* { dg-final { scan-file "temp.json" "\"ldp_policy_model\": \"AARCH64_LDP_STP_POLICY_ALWAYS\"" } } */
/* { dg-final { scan-file "temp.json" "\"stp_policy_model\": \"AARCH64_LDP_STP_POLICY_ALWAYS\"" } } */

/* Check boolean values */
/* { dg-final { scan-file "temp.json" "\"non_exec_costs_exec\": true" } } */
/* { dg-final { scan-file "temp.json" "\"prefetch_dynamic_strides\": true" } } */

/* Check nested array values (mult costs) */
/* { dg-final { scan-file "temp.json" "\"simple\": 4" } } */
/* { dg-final { scan-file "temp.json" "\"idiv\": 24" } } */

int main () {} 