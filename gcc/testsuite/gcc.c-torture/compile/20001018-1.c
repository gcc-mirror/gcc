void
foo (void)
{
  extern char i[10];

  {
    extern char i[];
    char x[sizeof (i) == 10 ? 1 : -1];
  }
}
