// { dg-do link }
// { dg-options "-Os" }
/* PR target/28014: main references unsigned divide, and the unwinder
   references signed divide.
   If libgcc contains an object which defines both, and linking is done with
   a space-optimized library that defines these functions in separate objects,
   you end up with the function for unsigned divide defined twice.  */
int
main (int c, char **argv)

{
  return 0xffffU/c;
}
