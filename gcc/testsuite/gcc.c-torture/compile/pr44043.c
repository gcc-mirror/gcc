typedef unsigned char __u8;
typedef unsigned short __u16;
typedef unsigned int __u32;
typedef unsigned long __kernel_size_t;
typedef __kernel_size_t size_t;
typedef __u8 uint8_t;
typedef __u16 __be16;
typedef __u32 __be32;
struct msghdr {
    struct iovec * msg_iov;
    unsigned msg_flags;
};
enum { IPPROTO_ICMP = 1 };
struct sk_buff { };
static inline __attribute__((always_inline)) struct dst_entry *
skb_dst(const struct sk_buff *skb)
{
};
enum nf_inet_hooks { NF_INET_LOCAL_OUT };
struct net_device {
    unsigned mtu;
};
static inline __attribute__((always_inline)) int
NF_HOOK_THRESH(uint8_t pf, unsigned int hook, struct sk_buff *skb,
	       struct net_device *in, struct net_device *out,
	       int (*okfn)(struct sk_buff *), int thresh)
{
  int ret = nf_hook_thresh(pf, hook, skb, in, out, okfn, thresh);
  if (ret == 1)
    ret = okfn(skb);
  return ret;
}
static inline __attribute__((always_inline)) int
NF_HOOK(uint8_t pf, unsigned int hook, struct sk_buff *skb,
	struct net_device *in, struct net_device *out,
	int (*okfn)(struct sk_buff *))
{
  return NF_HOOK_THRESH(pf, hook, skb, in, out, okfn, (-((int)(~0U>>1)) - 1));
}
struct dst_entry {
    struct net_device *dev;
    int (*output)(struct sk_buff*);
};
static inline __attribute__((always_inline)) int dst_output(struct sk_buff *skb) {
    return skb_dst(skb)->output(skb);
};
struct iphdr {
    __u8 protocol;
};
struct inet_sock {
    __be16 inet_dport;
    __u8 recverr: 1,     hdrincl: 1;
    struct { } cork;
};
struct icmphdr {
    __u8 type;
};
struct rtable {
    union { struct dst_entry dst; } u;
    __be32 rt_dst;
};
struct sock;
struct inet_sock *inet_sk (struct sock *);
struct net *sock_net (struct sock *);
void *skb_transport_header (struct sk_buff *);
static int raw_send_hdrinc(struct sock *sk, void *from, size_t length,
			   struct rtable *rt,    unsigned int flags)
{
  struct inet_sock *inet = inet_sk(sk);
  struct net *net = sock_net(sk);
  struct iphdr *iph;
  struct sk_buff *skb;
  if (length > rt->u.dst.dev->mtu) 
    ip_local_error(sk, 90, rt->rt_dst, inet->inet_dport, rt->u.dst.dev->mtu);
  if (flags&0x10)
    goto out;
  if (iph->protocol == IPPROTO_ICMP)
    icmp_out_count(net, ((struct icmphdr *)skb_transport_header(skb))->type);
  NF_HOOK(2, NF_INET_LOCAL_OUT, skb, ((void *)0), rt->u.dst.dev,
	  dst_output);
out:
  while (0);
}
int raw_sendmsg(struct sock *sk, struct msghdr *msg, size_t len)
{
  raw_send_hdrinc(sk, msg->msg_iov, len, (void *)0, msg->msg_flags);
}
