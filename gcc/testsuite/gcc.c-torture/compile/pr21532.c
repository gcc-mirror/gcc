

int
bar (unsigned char key)
{
  unsigned char buf[sizeof (unsigned long)+2];
  unsigned char b;
  unsigned char *buf_ = buf + 1;

  for (b = 8; b != 0; b--)
    buf_[b] = key >> b;

  return foo (b);
}
