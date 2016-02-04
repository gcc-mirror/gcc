/* PR target/pr68400
   This was triggering an ICE in change_address_1 when compiled with -Os.  */

/* { dg-do compile } */
/* { dg-options "-fstack-protector -mips16" } */

typedef struct s {
 unsigned long long d;
 long long t;
} p;

int sh(int x, unsigned char *buf)
{
 p *uhdr = (p *)buf;
 unsigned int i = 0;
 uhdr->d = ((uhdr->d & 0xff00000000000000LL) >> 56)
            | ((uhdr->d & 0x0000ff0000000000LL) >> 24)
            | ((uhdr->d & 0x00000000ff000000LL) << 8)
            | ((uhdr->d & 0x00000000000000ffLL) << 56);
 uhdr->t = ((uhdr->t & 0xff00000000000000LL) >> 56)
                | ((uhdr->t & 0x0000ff0000000000LL) >> 24)
                | ((uhdr->t & 0x000000ff00000000LL) >> 8)
                | ((uhdr->t & 0x00000000ff000000LL) << 8)
                | ((uhdr->t & 0x000000000000ff00LL) << 40)
                | ((uhdr->t & 0x00000000000000ffLL) << 56);
 i += 4;
 if (x < i) return 0; else return 1;
}
