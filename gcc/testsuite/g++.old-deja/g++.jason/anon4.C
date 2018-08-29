// { dg-do assemble  }
// { dg-options "-O" }
// PRMS Id: 5371
// Bug: g++ screws up the alignment of buff and dies.

int
main()
{
  union {
    double a;
    char buff[sizeof(double)];
  };

  void *p = buff;
  return 0;
}
