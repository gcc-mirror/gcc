typedef unsigned char __u8;
struct sk_buff
{
  unsigned char *data;
};
struct cpl_pass_accept_req
{
  __u8 : 6;
  __u8 sack : 1;
};
void build_cpl_pass_accept_req(struct sk_buff* skb)
{
  struct cpl_pass_accept_req* req;
  skb->data -= sizeof(*req);
  req = (struct cpl_pass_accept_req *)skb->data;
  req->sack = 1;
}
