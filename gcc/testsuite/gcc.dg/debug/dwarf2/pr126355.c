/* PR debug/126355 */
/* Verify that an IPA ICF wrapper for an address-taken function keeps an
   address-bearing subprogram DIE.  */
/* { dg-do compile } */
/* { dg-options "-O2 -g -gdwarf -dA -fdump-ipa-icf-details" } */

int
pr_icf_wrapper_a (const char *host)
{
  (void) host;
  return 0;
}

int
pr_icf_wrapper_b (const char *host)
{
  (void) host;
  return 0;
}

int (*keep_a) (const char *) = pr_icf_wrapper_a;
int (*keep_b) (const char *) = pr_icf_wrapper_b;

int
main (int argc, char **argv)
{
  const char *arg = argc > 1 ? argv[1] : "x";
  return keep_a (arg) + keep_b (arg);
}

/* Check the ICF direction explicitly because the DWARF scan below inspects
   pr_icf_wrapper_b, the wrapper.  */
/* { dg-final { scan-ipa-dump "Semantic equality hit:pr_icf_wrapper_a/\[0-9+\]+->pr_icf_wrapper_b/\[0-9+\]+" "icf" } } */
/* { dg-final { scan-ipa-dump "Wrapper has been created" "icf" } } */
/* { dg-final { scan-assembler "\\(DIE \\(0x\[0-9a-f\]+\\) DW_TAG_subprogram\\)\[\r\n\]+(\[^\r\n\]*\[\r\n\]+){1,12}\[^\r\n\]*DW_AT_name: \"pr_icf_wrapper_b\"\[\r\n\]+(\[^\r\n\]*\[\r\n\]+){1,12}\[^\r\n\]*DW_AT_low_pc" } } */
