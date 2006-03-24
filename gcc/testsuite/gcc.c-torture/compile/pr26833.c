void yasm_lc3b__parse_insn( int num_info, int *num_operands
 , int *operands, int op)
{
  int found = 0;
  int i;
  for (; num_info>0 && !found; num_info--)
   {
    int mismatch = 0;
     for(i = 0;op && (i<*num_operands)&& !mismatch; i++)
     {
       if (!(int)(operands[i] & 0x1))
         mismatch = 1;
       if (mismatch)
         break;
     }
    if (!mismatch)
      found = 1;
  }
}
