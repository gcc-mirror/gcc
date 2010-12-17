/* Test that 'encode', 'end', 'compatibility_alias', 'defs',
   'protocol', 'selector', finally', 'synchronized', 'interface',
   'implementation' are not keywords if not after a '@'.
*/
/* { dg-do compile } */

int encode (int end)
{
  int compatibility_alias = end * 2;
  int defs = compatibility_alias * 2;
  int protocol = defs * 2;
  int selector = protocol * 2;
  int finally = selector * 2;
  int synchronized = finally * 2;
  int interface = synchronized * 2;
  int implementation = interface * 2;

  return implementation;
}

int main (void)
{
  return encode (0);
}
