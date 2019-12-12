/* Check that sizeof(bool) is 4 if we don't use special options. */
/* Matt Austern  <austern@apple.com> */
/* { dg-do run { target { powerpc*-*-darwin* && ilp32 } } } */
/* We do need to suppress the ISO C doesn't support _Bool message tho.  */
/* { dg-options "-Wno-pedantic" } */

int dummy1[sizeof(_Bool) - 3];
int dummy2[5 - sizeof(_Bool)];

int main()
{
  return sizeof(_Bool) == 4 ? 0 : 1;
}
