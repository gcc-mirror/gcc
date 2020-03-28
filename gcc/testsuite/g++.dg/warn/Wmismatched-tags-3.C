/* { dg-do compile }
   { dg-options "-Wall -Wmismatched-tags" } */

extern class C1 c1;             // { dg-message "declared as 'class'" }
extern struct C1 c1;            // { dg-warning "\\\[-Wmismatched-tags" }

void fs1 (struct S1);           // { dg-message "declared as 'struct'" }
void fs1 (class S1);            // { dg-warning "\\\[-Wmismatched-tags" }

enum
{
  ec2 = sizeof (struct C2*),    // { dg-message "declared as 'struct'" }
  fc2 = sizeof (class C2*)      // { dg-warning "\\\[-Wmismatched-tags" }
};
