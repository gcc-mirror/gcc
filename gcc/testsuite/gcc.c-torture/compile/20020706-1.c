// Contributed by Alexandre Oliva <aoliva@redhat.com>
// From Red Hat case 106165.

/* { dg-require-effective-target indirect_calls } */

typedef struct s1
{
  unsigned short v1;
  unsigned char *v2;
} S1;

extern void bar(const struct s1 *const hdb);
extern unsigned char* foo ();

unsigned int sn;
S1 *hdb;
S1 *pb;
unsigned short len;

unsigned int crashIt()
{
  unsigned char *p;
  unsigned int nsn;
  unsigned short cnt;

  if (sn != 0) return 1;

  if ((len < 12) || ((p = (((pb->v1) >= 8) ? pb->v2 : foo() )) == 0))
    return 1;

  nsn = (
	 (((*(unsigned int*)p) & 0x000000ff) << 24) |
	 (((*(unsigned int*)p) & 0x0000ff00) << 8)  |
	 (((*(unsigned int*)p) & 0x00ff0000) >> 8)  |
	 (((*(unsigned int*)p) & 0xff000000) >> 24)  );
  p += 4;

  cnt = (unsigned short) ((
			   (((*(unsigned int*)p) & 0x000000ff) << 24) |
			   (((*(unsigned int*)p) & 0x0000ff00) << 8)  |
			   (((*(unsigned int*)p) & 0x00ff0000) >> 8)  |
			   (((*(unsigned int*)p) & 0xff000000) >> 24)  ) &
			  0xffff);

  if ((len != 12 + (cnt * 56)) || (nsn == 0))
    {
      bar(hdb);
      return 1;
    }

  return 0;
}
