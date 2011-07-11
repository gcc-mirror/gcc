/* { dg-do run } */
/* { dg-options "-O" } */

struct in_addr {
	unsigned int s_addr;
};

struct ip {
	unsigned char ip_p;
	unsigned short ip_sum;
	struct	in_addr ip_src,ip_dst;
} __attribute__ ((aligned(1), packed));

struct ip ip_fw_fwd_addr;

int test_alignment( char *m )
{
  struct ip *ip = (struct ip *) m;
  struct in_addr pkt_dst;
  pkt_dst = ip->ip_dst ;
  if( pkt_dst.s_addr == 0 )
    return 1;
  else
    return 0;
}

int __attribute__ ((noinline, noclone))
intermediary (char *p)
{
  return test_alignment (p);
}

int
main (int argc, char *argv[])
{
  ip_fw_fwd_addr.ip_dst.s_addr = 1;
  return intermediary ((void *) &ip_fw_fwd_addr);
}
