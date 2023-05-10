/* Check that PRFM-related bounds checks are applied correctly.  */
/* { dg-do compile } */
#include <arm_acle.h>

/* Access kind specifiers.  */
#define KIND_LOW -1
#define KIND_HIGH 2
/* Cache levels.  */
#define LEVEL_LOW  -1
#define LEVEL_HIGH  4
/* Retention policies.  */
#define POLICY_LOW -1
#define POLICY_HIGH 2

void
data_rw_prefetch_bad_bounds (void *a)
{
  __builtin_aarch64_pldx (KIND_LOW, 0, 0, a);  /* { dg-error {argument 1 must be a constant immediate in range \[0,1\]} } */
  __builtin_aarch64_pldx (KIND_HIGH, 0, 0, a); /* { dg-error {argument 1 must be a constant immediate in range \[0,1\]} } */
  __builtin_aarch64_pldx (0, LEVEL_LOW, 0, a);  /* { dg-error {argument 2 must be a constant immediate in range \[0,3\]} } */
  __builtin_aarch64_pldx (0, LEVEL_HIGH, 0, a); /* { dg-error {argument 2 must be a constant immediate in range \[0,3\]} } */
  __builtin_aarch64_pldx (0, 0, POLICY_LOW, a);  /* { dg-error {argument 3 must be a constant immediate in range \[0,1\]} } */
  __builtin_aarch64_pldx (0, 0, POLICY_HIGH, a); /* { dg-error {argument 3 must be a constant immediate in range \[0,1\]} } */
}

void
insn_prefetch_bad_bounds (void *a)
{
  __builtin_aarch64_plix (LEVEL_LOW, 0, a);  /* { dg-error {argument 1 must be a constant immediate in range \[0,3\]} } */
  __builtin_aarch64_plix (LEVEL_HIGH, 0, a); /* { dg-error {argument 1 must be a constant immediate in range \[0,3\]} } */
  __builtin_aarch64_plix (0, POLICY_LOW, a);  /* { dg-error {argument 2 must be a constant immediate in range \[0,1\]} } */
  __builtin_aarch64_plix (0, POLICY_HIGH, a); /* { dg-error {argument 2 must be a constant immediate in range \[0,1\]} } */
}
