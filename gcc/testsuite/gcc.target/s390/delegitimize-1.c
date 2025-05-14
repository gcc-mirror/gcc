/* { dg-do compile } */
/* { dg-options "-m64 -fPIC -O2 -g -fdump-rtl-final-details" } */

struct sk_buff {
  struct {
    struct {
      struct {
        int inner_ipproto;
      };
    };
  };
};
void skb_udp_tunnel_segment(struct sk_buff *skb);
const int *inet_offloads[42], *inet6_offloads[42];
_Bool skb_udp_tunnel_segment_is_ipv6;
void skb_udp_tunnel_segment(struct sk_buff *skb) {
  const int **offloads =
      skb_udp_tunnel_segment_is_ipv6 ? inet6_offloads : inet_offloads;
  *(volatile typeof(_Generic(0, default : 0)) *)&offloads[skb->inner_ipproto];
}

/* { dg-final { scan-rtl-dump-not "Failed to expand as dwarf:" "final" } } */
