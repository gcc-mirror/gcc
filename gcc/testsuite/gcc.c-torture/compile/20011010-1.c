extern int used (void);


int foo () 
{
  int i;
  for (; used (); ({while (1) if (used ()) return 0;}))
    i++;
  return i;
}
