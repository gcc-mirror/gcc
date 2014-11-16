int _IO_getc(int*);
read_long(int *fp)
{
  unsigned char b0, b1, b2, b3;
  b0 = _IO_getc (fp);
  b1 = _IO_getc (fp);
  b2 = _IO_getc (fp);
  b3 = _IO_getc (fp);
  return ((int)(((((b3 << 8) | b2) << 8) | b1) << 8) | b0);
}
