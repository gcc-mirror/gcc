#ifndef NO_LABEL_VALUES
int
x (int i)
{
  static const void *j[] = {&& x, && y, && z};
  
  goto *j[i];
  
 x: return 2;
 y: return 3;
 z: return 5;
}

int
main (void)
{
  if (   x (0) != 2
      || x (1) != 3
      || x (2) != 5)
    abort ();

  exit (0);
}
#else
int
main (void) { exit (0); }
#endif
