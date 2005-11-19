void foo(void);
void
bar (unsigned char *p)
{
  int j;
  j = *(p) ;
  j += ((signed char) (*p) ) << 8;
  if (j)
    foo();
}
