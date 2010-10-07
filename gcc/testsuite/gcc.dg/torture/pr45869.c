/* { dg-do compile } */

char *
foo (unsigned int count, void **list)
{
  char *minaddr = (char *) list[0];
  unsigned int i; /* NOTE: change of type to "int" eliminates the ICE */
  for (i = 1; i < count; i++)
    {
      char *addr = (char *) list[i];
      if (addr < minaddr)
	minaddr = addr;
    }
  return minaddr;
}
