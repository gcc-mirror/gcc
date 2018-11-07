extern void *memcpy(void *, const void *, __SIZE_TYPE__);
struct in6_addr {
  struct {
    int u6_addr32[4];
  };
};
struct foo {
  struct in6_addr daddr;
  struct in6_addr saddr;
} a;
extern void ip6_route_output(struct foo, int);
int b;
static void find_route_ipv6(struct in6_addr *p1) {
  if (p1)
    memcpy(0, p1, sizeof(struct in6_addr));
  ip6_route_output(a, b);
}
void cxgbi_ep_connect() { find_route_ipv6(0); }

