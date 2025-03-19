/* Verify that we don't crash if we bail out with a fatal error
   while an on-stack attribute_urlifier is active (PR c/119366).  */
/* { dg-options "-Wfatal-errors -Werror=attributes" } */
void foo() __attribute__((this_does_not_exist)); /* { dg-error "'this_does_not_exist' attribute directive ignored" } */

/* { dg-message "some warnings being treated as errors" "treated as errors" {target "*-*-*"} 0 } */
/* { dg-message "terminated due to -Wfatal-errors" "terminated" { target *-*-* } 0 } */

