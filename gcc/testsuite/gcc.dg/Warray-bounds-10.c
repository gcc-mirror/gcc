/* { dg-do compile } */
/* { dg-options "-O3 -Warray-bounds" } */

int f(unsigned len, int buflen)
{
  unsigned taillen;
  unsigned slen;
  unsigned i;
  int b[17];            /* needed <= 17 to trigger Warning */
  int j = 0;            /* needed to trigger Warning */

  b[0] = 0;
  taillen= buflen & 7;    /* taillen [0..7] */

  if(taillen) {        /* taillen [1..7] */
      slen= 8 - taillen;    /* slen    [7..1] */
      if (len<slen)        /* needed to trigger Warning  */
	slen=len;        /* slen' < slen  */
      for(i=0; i<slen; i++) {
	  j = b[taillen];    /* taillen + slen = [1..7] + [7..1] = 8 */
	  taillen++;
      }
  }
  return j;
}
