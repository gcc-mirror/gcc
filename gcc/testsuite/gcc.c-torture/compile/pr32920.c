int a;
void
foo(void)
{
  char buf[10];
  a = a < sizeof(buf) ? a : sizeof (buf);
}
