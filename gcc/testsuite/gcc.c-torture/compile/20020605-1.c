/* This testcase caused on IA-32 -O2 endless loop in
   merge_blocks when trying to merge a basic block
   with itself.  */

void f (void)
{
  char *c;
  do
    {
      if (c)
	break;
    }
  while (1);
  if (!c)
    while (1)
      f ();
}
