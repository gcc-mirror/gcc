/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

byte_match_count2 (buf, n, xm, m1, m2, m3, m4)
     unsigned *buf;
     unsigned n;
     unsigned xm;
     unsigned m1, m2, m3, m4;
{
  unsigned w, cnt = 0;
  unsigned *bp;

  n /= 4;

  bp = buf;
  while (bp < buf + n)
    {
      w = *bp++;
      w ^= xm;
      cnt += ((m1 & w) == 0);
      cnt += ((m2 & w) == 0);
      cnt += ((m3 & w) == 0);
      cnt += ((m4 & w) == 0);

      w = *bp++;
      w ^= xm;
      cnt += ((m1 & w) == 0);
      cnt += ((m2 & w) == 0);
      cnt += ((m3 & w) == 0);
      cnt += ((m4 & w) == 0);

      w = *bp++;
      w ^= xm;
      cnt += ((m1 & w) == 0);
      cnt += ((m2 & w) == 0);
      cnt += ((m3 & w) == 0);
      cnt += ((m4 & w) == 0);

      w = *bp++;
      w ^= xm;
      cnt += ((m1 & w) == 0);
      cnt += ((m2 & w) == 0);
      cnt += ((m3 & w) == 0);
      cnt += ((m4 & w) == 0);
    }
  return cnt;
}
