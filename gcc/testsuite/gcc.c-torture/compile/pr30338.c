/* We used to do folding with mismatched types which caused us to
   infinitely loop in comparison foldings.  */

extern char *grub_scratch_mem;
int testload_func (char *arg, int flags)
{
  int i;
  for (i = 0; i < 0x10ac0; i++)
    if (*((unsigned char *) ((0x200000 + i + (int) grub_scratch_mem)))
        != *((unsigned char *) ((0x300000 + i + (int) grub_scratch_mem))))
      return 0;
  return 1;
}

