// PRMS Id: 5371
// Bug: g++ screws up the alignment of buff and dies.
// Build don't link:
// Special g++ Options: -O

main()
{
  union {
    double a;
    char buff[sizeof(double)];
  };

  void *p = buff;
}
