/* PR tree-optimization/83456 - -Wrestrict false positive on
   a non-overlapping memcpy in an inline function
   { dg-do compile }
   { dg-options "-O2 -Wrestrict -ftrack-macro-expansion=0" }  */

extern void* memcpy (void*, const void*, __SIZE_TYPE__);

/* Test case from comment #0.  */

inline void pr83456_comment0 (void *d, void *s, unsigned N)
{
  if (s != d)
    memcpy (d, s, N);
}

void call_pr83456_comment0 (void* src)
{
  pr83456_comment0 (src, src, 1);
}


/* Test case from comment #1.  */

char a[4];

void pr83456_comment1 (unsigned n)
{
  for (int i = 0; i < 1; i++)
    {
      if (!i)
	continue;

      memcpy (a, a, n);
    }
}

/* Test case from comment #2.  */

struct netdevice {
  void *priv;
};

struct ip_tunnel {
  struct netdevice *dev;
  int ip6rd[3];
};

struct sit_net {
  struct netdevice *fb_tunnel_dev;
};

void ipip6_tunnel_clone_6rd (struct netdevice *dev, struct sit_net *sitn)
{
  struct ip_tunnel *t = dev->priv;
  if (t->dev == sitn->fb_tunnel_dev)
    return;

  struct ip_tunnel *t0 = sitn->fb_tunnel_dev->priv;
  memcpy(&t->ip6rd, &t0->ip6rd, sizeof(t->ip6rd));
}

void sit_init_net (struct sit_net *sitn, struct netdevice *fb_tunnel_dev)
{
  sitn->fb_tunnel_dev = fb_tunnel_dev;
  ipip6_tunnel_clone_6rd (sitn->fb_tunnel_dev, sitn);
}
