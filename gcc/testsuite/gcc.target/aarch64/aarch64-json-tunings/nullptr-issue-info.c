/* Test that a structure provided in JSON is parsed even when the base tuning
   model has the structure set to NULL.  */

/* { dg-do compile } */
/* { dg-additional-options "-muser-provided-CPU=${srcdir}/gcc.target/aarch64/aarch64-json-tunings/nullptr-issue-info.json -fdump-tuning-model=temp-nullptr-issue-info.json" } */

/* { dg-warning "JSON tuning overrides an unspecified structure in the base tuning; fields not provided in JSON will default to 0" "" { target *-*-* } 0 } */
/* { dg-warning "JSON tuning file does not contain version information" "" { target *-*-* } 0 } */

/* { dg-final { scan-file "temp-nullptr-issue-info.json" "\"loads_stores_per_cycle\": 4" } } */
/* { dg-final { scan-file "temp-nullptr-issue-info.json" "\"general_ops_per_cycle\": 8" } } */

/* { dg-final { scan-file "temp-nullptr-issue-info.json" "\"ld2_st2_general_ops\": 2" } } */
/* { dg-final { scan-file "temp-nullptr-issue-info.json" "\"ld4_st4_general_ops\": 3" } } */

/* { dg-final { scan-file "temp-nullptr-issue-info.json" "\"pred_ops_per_cycle\": 2" } } */
/* { dg-final { scan-file "temp-nullptr-issue-info.json" "\"while_pred_ops\": 1" } } */
/* { dg-final { scan-file "temp-nullptr-issue-info.json" "\"gather_scatter_pair_general_ops\": 1" } } */

int main () {}
