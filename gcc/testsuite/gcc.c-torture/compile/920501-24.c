char *malloc ();

main ()
{
  char *buf1;
  int i;
  int cnt;

  buf1 = malloc (8193);

  read (0, buf1, 8192);

  for (i = 500;  i > 0;  i--)
    cnt = shift (buf1, 8192, '\n');
  printf ("%d  ", cnt);

  for (i = 500;  i > 0;  i--)
    cnt = shift_xor (buf1, 8192, '\n');
  printf ("%d  ", cnt);

  for (i = 500;  i > 0;  i--)
    cnt = bitfield (buf1, 8192, '\n');
  printf ("%d  ", cnt);

  for (i = 500;  i > 0;  i--)
    cnt = char_autoincr (buf1, 8192, '\n');
  printf ("%d  ", cnt);

  for (i = 500;  i > 0;  i--)
    cnt = xorand (buf1, 8192, '\n');
  printf ("%d  ", cnt);
}

shift (b1, n, c)
    int *b1;
    int n;
    char c;
{
  int nn;
  int cnt = 0;
  int w1;

  for (nn = n / (4 * 4);  nn > 0;  nn--)
    {
      w1 = *b1++;
      if ((char)w1 == c)  cnt++;
      if ((char)(w1 >>= 8) == c)  cnt++;
      if ((char)(w1 >>= 8) == c)  cnt++;
      if ((char)(w1 >>= 8) == c)  cnt++;
      w1 = *b1++;
      if ((char)w1 == c)  cnt++;
      if ((char)(w1 >>= 8) == c)  cnt++;
      if ((char)(w1 >>= 8) == c)  cnt++;
      if ((char)(w1 >>= 8) == c)  cnt++;
      w1 = *b1++;
      if ((char)w1 == c)  cnt++;
      if ((char)(w1 >>= 8) == c)  cnt++;
      if ((char)(w1 >>= 8) == c)  cnt++;
      if ((char)(w1 >>= 8) == c)  cnt++;
      w1 = *b1++;
      if ((char)w1 == c)  cnt++;
      if ((char)(w1 >>= 8) == c)  cnt++;
      if ((char)(w1 >>= 8) == c)  cnt++;
      if ((char)(w1 >>= 8) == c)  cnt++;
    }
  return cnt;
}

shift_xor (b1, n, c)
    int *b1;
    int n;
    char c;
{
  int nn;
  int cnt = 0;
  int w1;
  int cccc = (c << 24) | (c << 16) | (c << 8) | c;

  for (nn = n / (4 * 4);  nn > 0;  nn--)
    {
      w1 = *b1++ ^ cccc;
      if ((char)w1 == 0)  cnt++;
      if ((char)(w1 >>= 8) == 0)  cnt++;
      if ((char)(w1 >>= 8) == 0)  cnt++;
      if ((char)(w1 >>= 8) == 0)  cnt++;
      w1 = *b1++ ^ cccc;
      if ((char)w1 == 0)  cnt++;
      if ((char)(w1 >>= 8) == 0)  cnt++;
      if ((char)(w1 >>= 8) == 0)  cnt++;
      if ((char)(w1 >>= 8) == 0)  cnt++;
      w1 = *b1++ ^ cccc;
      if ((char)w1 == 0)  cnt++;
      if ((char)(w1 >>= 8) == 0)  cnt++;
      if ((char)(w1 >>= 8) == 0)  cnt++;
      if ((char)(w1 >>= 8) == 0)  cnt++;
      w1 = *b1++ ^ cccc;
      if ((char)w1 == 0)  cnt++;
      if ((char)(w1 >>= 8) == 0)  cnt++;
      if ((char)(w1 >>= 8) == 0)  cnt++;
      if ((char)(w1 >>= 8) == 0)  cnt++;
    }
  return cnt;
}

typedef
struct
{
  unsigned b0:8;
  unsigned b1:8;
  unsigned b2:8;
  unsigned b3:8;
} foo;

bitfield (b1, n, c)
    foo *b1;
    int n;
    char c;
{
  int nn;
  int cnt = 0;
  register foo w1;

  for (nn = n / (4 * 4);  nn > 0;  nn--)
    {
      w1 = *b1++;
      if (w1.b0 == c) cnt++;
      if (w1.b1 == c) cnt++;
      if (w1.b2 == c) cnt++;
      if (w1.b3 == c) cnt++;
      w1 = *b1++;
      if (w1.b0 == c) cnt++;
      if (w1.b1 == c) cnt++;
      if (w1.b2 == c) cnt++;
      if (w1.b3 == c) cnt++;
      w1 = *b1++;
      if (w1.b0 == c) cnt++;
      if (w1.b1 == c) cnt++;
      if (w1.b2 == c) cnt++;
      if (w1.b3 == c) cnt++;
      w1 = *b1++;
      if (w1.b0 == c) cnt++;
      if (w1.b1 == c) cnt++;
      if (w1.b2 == c) cnt++;
      if (w1.b3 == c) cnt++;
    }
  return cnt;
}


char_autoincr (b1, n, c)
    char *b1;
    int n;
    char c;
{
  int cnt = 0;
  char *bend = b1 + n;
  *bend = c;

  for (;;)
    {
      while (*b1++ != c)
	;
      cnt++;
      if (b1 >  bend)
	break;
    }
  return cnt;
}

typedef unsigned int uint;

xorand (b1, n, c)
     int *b1;
     int n;
     unsigned char c;
{
  int xm = ((int) c << 24) | ((int) c << 16) | ((int) c << 8) | ((int) c);
  byte_match_count2 (b1, n, xm, 0xff000000, 0xff0000, 0xff00, 0xff);
}

byte_match_count2 (buf, n, xm, m1, m2, m3, m4)
     int *buf;
     int n;
     uint m1, m2, m3, m4;
{
  int w, cnt = 0;
  int *bp;

  n /= 4;

  buf[n] = xm;

  bp = buf;
  while (bp < buf + n)
    {
      w = *bp++ ^ xm;
      cnt += ((m1 & w) == 0);
      cnt += ((m2 & w) == 0);
      cnt += ((m3 & w) == 0);
      cnt += ((m4 & w) == 0);

      w = *bp++ ^ xm;
      cnt += ((m1 & w) == 0);
      cnt += ((m2 & w) == 0);
      cnt += ((m3 & w) == 0);
      cnt += ((m4 & w) == 0);

      w = *bp++ ^ xm;
      cnt += ((m1 & w) == 0);
      cnt += ((m2 & w) == 0);
      cnt += ((m3 & w) == 0);
      cnt += ((m4 & w) == 0);

      w = *bp++ ^ xm;
      cnt += ((m1 & w) == 0);
      cnt += ((m2 & w) == 0);
      cnt += ((m3 & w) == 0);
      cnt += ((m4 & w) == 0);
    }
  return cnt;
}
