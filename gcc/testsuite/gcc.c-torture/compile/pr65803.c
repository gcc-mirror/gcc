/* { dg-options "-fno-strict-overflow" } */
typedef unsigned char __uint8_t;
typedef __uint8_t uint8_t;
typedef uint8_t u8_t;
typedef struct ip_addr ip_addr_t;
char *
ipaddr_ntoa_r (const ip_addr_t * addr, char *buf, int buflen)
{
  char inv[3];
  char *rp;
  u8_t *ap;
  u8_t n;
  u8_t i;
  int len = 0;
  for (n = 0; n < 4; n++)
    {
      while (*ap);
      while (i--)
	{
	  if (len++ >= buflen)
	    return ((void *) 0);
	  *rp++ = inv[i];
	} ap++;
    }
}
