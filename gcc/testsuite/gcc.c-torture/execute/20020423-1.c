/* PR c/5430 */
/* Verify that the multiplicative folding code is not fooled
   by the mix between signed variables and unsigned constants. */

extern void abort (void);
extern void exit (int);

int main (void)
{
  int my_int = 924;
  unsigned int result;

  result = ((my_int*2 + 4) - 8U) / 2;
  if (result != 922U)
    abort();
         
  result = ((my_int*2 - 4U) + 2) / 2;
  if (result != 923U)
    abort();

  result = (((my_int + 2) * 2) - 8U - 4) / 2;
  if (result != 920U)
    abort();
  result = (((my_int + 2) * 2) - (8U + 4)) / 2;
  if (result != 920U)
    abort();

  result = ((my_int*4 + 2U) - 4U) / 2;
  if (result != 1847U)
    abort();

  exit(0);
}
