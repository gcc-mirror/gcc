foo (p)
     short *p;
{
  static int *foo;
  *p = 1234;
  *foo = 1234;
}
