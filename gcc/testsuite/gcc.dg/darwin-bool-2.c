/* Check that sizeof(bool) is 1 if we use the -mone-byte-bool option. */
/* Matt Austern  <austern@apple.com> */
/* { dg-do run { target powerpc*-*-darwin* } } */
/* { dg-options "-mone-byte-bool" } */

int dummy1[sizeof(_Bool)];
int dummy2[2 - sizeof(_Bool)];

int main()
{
  return sizeof(_Bool) == 1 ? 0 : 1;
}
