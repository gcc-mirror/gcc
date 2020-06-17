/* A non-recursive example of state-merger of a pointer
   from an old stack frame to a local in a newer stack frame.  */

int newer (int **ptr_to_ow, int flag);

int
older (int flag)
{
  int *ow;
  return newer (&ow, flag);
}

int
newer (int **ptr_to_ow, int flag)
{
  int pk;
  *ptr_to_ow = &pk;
  
  if (flag)
    pk = 3;
  else
    pk = 4;
  /* State merger.  */

  return pk;
}
