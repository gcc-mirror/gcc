int giop_tx_big_endian;

inline
void
giop_encode_ulong (unsigned long i, char *buf)
{
  if (giop_tx_big_endian)
    {
      *(unsigned long *) buf = i;
    }
  else
    {
      *buf++ = i & 0xff;
      *buf++ = (i >> 8) & 0xff;
      *buf++ = (i >> 16) & 0xff;
      *buf = (i >> 24) & 0xff;
    }
}



static
double
time_giop_encode (unsigned long l)
{
  int c;
  char buf[4];

  for (c = 0; c < (512 * 1024 * 1024); ++c)
    {
      giop_encode_ulong (l, buf);
    }
}

int
main (int ac, char *av[])
{
  giop_tx_big_endian = 1;
  time_giop_encode (0);
}
