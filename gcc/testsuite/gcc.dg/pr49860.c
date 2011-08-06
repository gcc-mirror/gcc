/* { dg-do assemble } */
/* { dg-options "-O3 -funroll-all-loops" } */

extern char inbuf[];
extern char outbuf[];
extern unsigned insize;
extern unsigned inptr;
static int max_len;
static int peek_bits;
void build_tree() {
  int len;
  char *prefixp;
  max_len = inbuf[inptr++];
  peek_bits = ((max_len) <= (12) ? (max_len) : (12));
  prefixp = &outbuf[1<<peek_bits];
  for (len = 1;
       len <= peek_bits;
       len++) {
  }
  while (prefixp > outbuf) *--prefixp = 0;
}
