/* { dg-do compile { target ia32 } }  */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -march=tigerlake -fPIC" } */
/* { dg-final { scan-assembler-not {(?n)kmovd.*@gotntpoff} } }  */

typedef unsigned short uint16_t;
typedef int int32_t;
typedef unsigned int uint32_t;
typedef unsigned char uint8_t;

typedef uint32_t in_addr_t;
struct in_addr { in_addr_t s_addr; };

extern __thread const uint16_t * __libc_tsd_CTYPE_B __attribute__ ((tls_model ("initial-exec")));
extern __thread int __libc_errno __attribute__ ((tls_model ("initial-exec")));

extern unsigned long strtoul (const char*, char**, int);
extern uint32_t __bswap_32 (in_addr_t);
int
inet_aton_end (const char *cp, struct in_addr *addr, const char **endp)
{
  static const in_addr_t max[4] = { 0xffffffff, 0xffffff, 0xffff, 0xff };
  in_addr_t val;
  char c;
  union iaddr
  {
    uint8_t bytes[4];
    uint32_t word;
  } res;
  uint8_t *pp = res.bytes;
  int digit;

  int saved_errno = __libc_errno;
  __libc_errno = 0;
  res.word = 0;
  c = *cp;

  for (;;)
    {
      if (c < '0' || c > '9')
	goto ret_0;
      {
	char *endp;
	unsigned long ul = strtoul (cp, &endp, 0);
	if (ul == 0x7fffffffL && __libc_errno == 34)
	  goto ret_0;
	if (ul > 0xfffffffful)
	  goto ret_0;
	val = ul;
	digit = cp != endp;
	cp = endp;
      }
      c = *cp;
      if (c == '.')
	{
	  if (pp > res.bytes + 2 || val > 0xff)
	    goto ret_0;
	  *pp++ = val;
	  c = *++cp;
	}
      else
	break;
    }

  if (!(__libc_tsd_CTYPE_B[(int)c] & 8192))
    goto ret_0;

  if (!digit)
    goto ret_0;

  if (val > max[pp - res.bytes])
    goto ret_0;

  if (addr != 0)
    addr->s_addr = res.word | __bswap_32 (val);
  *endp = cp;

  __libc_errno = saved_errno;
  return 1;

 ret_0:
  __libc_errno = saved_errno;
  return 0;
}
