struct X
{
  unsigned char buf[sizeof (int)];
};
void bar () { struct X x; *(volatile char *)x.buf = 1; }
