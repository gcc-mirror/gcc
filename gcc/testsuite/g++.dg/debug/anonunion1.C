// PR debug/9039
// Verify that the debugging backends don't get confused by ALIAS_DECLs.

int foo()
{
  union
  {
    int z;
    unsigned int w;
  };

  w = 0;
  return 0;
}
