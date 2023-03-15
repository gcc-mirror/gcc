/* { dg-do compile } */
/* { dg-additional-options "-ftrivial-auto-var-init=zero" } */

const int COMPARE_CANDIDATE;
char ipmi_ek_compare_link_record1_0, ipmi_ek_compare_link_record2_0;
void ipmi_ek_compare_link()
{
  for (; ipmi_ek_compare_link_record1_0;)
    for (; ipmi_ek_compare_link_record2_0;) {
	int link[COMPARE_CANDIDATE];
    }
}
