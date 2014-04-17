/* { dg-options "(-mmicromips) -dp" } */

MICROMIPS void
f1 (unsigned char *ptr)
{
  *ptr = 0;
}

MICROMIPS void
f2 (unsigned short *ptr)
{
  *ptr = 0;
}

MICROMIPS void
f3 (unsigned int *ptr)
{
  *ptr = 0;
}
/* { dg-final { scan-assembler "\tsb\t\\\$0,0\\(\\\$\[0-9\]+\\)\[^\n\]*length = 2" } } */
/* { dg-final { scan-assembler "\tsh\t\\\$0,0\\(\\\$\[0-9\]+\\)\[^\n\]*length = 2" } } */
/* { dg-final { scan-assembler "\tsw\t\\\$0,0\\(\\\$\[0-9\]+\\)\[^\n\]*length = 2" } } */
