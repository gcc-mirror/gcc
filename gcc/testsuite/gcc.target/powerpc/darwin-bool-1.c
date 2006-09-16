/* Check that sizeof(bool) is 4 if we don't use special options. */
/* Matt Austern  <austern@apple.com> */
/* { dg-do run { target powerpc*-*-darwin* } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "-m64" } { "" } } */

int dummy1[sizeof(_Bool) - 3];
int dummy2[5 - sizeof(_Bool)];

int main()
{
  return sizeof(_Bool) == 4 ? 0 : 1;
}
