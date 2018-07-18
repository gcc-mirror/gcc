/* { dg-options "-fmerge-all-constants" } */
/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

const char kRecoveryInstallString[] = "NEW";
const char kRecoveryUpdateString[] = "UPDATE";
const char kRecoveryUninstallationString1[] = "INSTALL";
const char kRecoveryUninstallationString2[] = "UNINSTALL";

volatile const int zero = 0;

int
main()
{
  char x1 = kRecoveryInstallString[zero + 0];
  char x2 = kRecoveryUpdateString[zero + 0];
  char x3 = kRecoveryUninstallationString1[zero + 0];
  char x4 = kRecoveryUninstallationString2[zero + 0];
  return (x1 + x2 + x3 + x4) == 0;
}
