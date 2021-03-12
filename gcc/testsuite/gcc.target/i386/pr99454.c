/* { dg-do compile } */
/* { dg-options "-O -w" } */

struct skb_shared_info {
  short gso_size;
};

enum { NETDEV_TX_OK };

struct iphdr {
  short tot_len;
  int daddr;
};

int tg3_tso_bug();
int netdev_priv();
int skb_cow_head();
int tcp_hdrlen__builtin_expect();
struct iphdr *ip_hdr();
int _tg3_flag();
int tg3_tso_bug_gso_check();

int
tg3_start_xmit() {
  int *tp = netdev_priv();
  int mss, tnapi;
  struct iphdr *iph;
  tnapi = mss = ((struct skb_shared_info *)0)->gso_size;
  if (mss) {
    int hdr_len;
    if (skb_cow_head())
      iph = ip_hdr();
    hdr_len = tcp_hdrlen__builtin_expect() && _tg3_flag();
    if (tg3_tso_bug_gso_check())
      return tg3_tso_bug(tp, tnapi);
    iph->tot_len = mss + hdr_len;
    if (_tg3_flag(tp) || tp)
      ;
    else
      asm("" : : "g"(iph->daddr));
  }
  return 0;
}
