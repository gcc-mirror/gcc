/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf-details"  } */

struct __hlist_head
{
  struct __hlist_node *first;
};

struct __hlist_node
{
  struct __hlist_node *next, **pprev;
};

struct __net
{
  int ifindex;
  struct __hlist_head * dev_index_head;
};

struct __net_device
{
  int ifindex;
  struct __net *nd_net;
  struct __hlist_node index_hlist;
};

__attribute__ ((noinline, noclone))
static struct __hlist_head * __dev_index_hash(struct __net *net,
    int ifindex)
{
  return &net->dev_index_head[ifindex & 1];
}

__attribute__ ((noinline, noclone))
struct __net_device * __dev_get_by_index(struct __net *net, int ifindex)
{
  struct __net_device * dev;
  struct __hlist_head * head = __dev_index_hash(net, ifindex);

  for (dev = ( { typeof((head)->first) ____ptr = ((head)->first); ____ptr ? ( { const typeof(((typeof(*(dev)) *) 0)->index_hlist) * __mptr = (____ptr); (typeof(*(dev)) *) ((char *)__mptr - __builtin_offsetof(typeof(*(dev)), index_hlist));}): ((void *) 0);});
       dev; dev = ( { typeof ((dev)->index_hlist.next) ____ptr = ((dev)->index_hlist.next); ____ptr ? ( { const typeof(((typeof(*(dev)) *) 0)->index_hlist) * __mptr = (____ptr); (typeof(*(dev)) *) ((char *)__mptr - __builtin_offsetof(typeof(*(dev)), index_hlist));}): ((void *) 0);}))
    if (dev->ifindex == ifindex)
      return dev;

  return ((void *)0);
}

__attribute__ ((noinline, noclone))
struct __net_device * dev_get_by_index_rcu(struct __net *net, int ifindex)
{
  struct __net_device * dev;
  struct __hlist_head * head = __dev_index_hash(net, ifindex);

  for (dev = ( { typeof(( { typeof (* ((*((struct __hlist_node **)(&(head)->first))))) * _________p1 = (typeof(*((*((struct __hlist_node **)(&(head)->first))))) *) (*(volatile typeof(((*((struct __hlist_node **)(&(head)->first))))) *)&(((*((struct __hlist_node **)(&(head)->first)))))); do { } while (0);; do { } while (0); ((typeof(*((*((struct __hlist_node **)(&(head)->first))))) *) (_________p1));})) ____ptr = (( { typeof (* ((*((struct __hlist_node **)(&(head)->first))))) * _________p1 = (typeof(*((*((struct __hlist_node **)(&(head)->first))))) *) (*(volatile typeof(((*((struct __hlist_node **)(&(head)->first))))) *)&(((*((struct __hlist_node **)(&(head)->first)))))); do { } while (0);; do { } while (0); ((typeof(*((*((struct __hlist_node **)(&(head)->first))))) *) (_________p1));})); ____ptr ? ( { const typeof(((typeof(*(dev)) *) 0)->index_hlist) * __mptr = (____ptr); (typeof(*(dev)) *) ((char *)__mptr - __builtin_offsetof(typeof(*(dev)), index_hlist));}):((void *) 0);});
	  dev; dev = ( { typeof(( { typeof (* ((*((struct __hlist_node **)(&(&(dev)->index_hlist)->next))))) * _________p1 = (typeof(*((*((struct __hlist_node **)(&(&(dev)->index_hlist)->next))))) *) (*(volatile typeof(((*((struct __hlist_node **)(&(&(dev)->index_hlist)->next))))) *)&(((*((struct __hlist_node **)(&(&(dev)->index_hlist)->next)))))); do { } while (0);; do { } while (0); ((typeof(*((*((struct __hlist_node **)(&(&(dev)->index_hlist)->next))))) *) (_________p1));})) ____ptr = (( { typeof (* ((*((struct __hlist_node **)(&(&(dev)->index_hlist)->next))))) * _________p1 = (typeof(*((*((struct __hlist_node **)(&(&(dev)->index_hlist)->next))))) *) (*(volatile typeof(((*((struct __hlist_node **)(&(&(dev)->index_hlist)->next))))) *)&(((*((struct __hlist_node **)(&(&(dev)->index_hlist)->next)))))); do { } while (0);; do { } while (0); ((typeof(*((*((struct __hlist_node **)(&(&(dev)->index_hlist)->next))))) *) (_________p1));})); ____ptr ? ( { const typeof(((typeof(*(dev)) *) 0)->index_hlist) * __mptr = (____ptr); (typeof(*(dev)) *) ((char *)__mptr - __builtin_offsetof(typeof(*(dev)), index_hlist));}): ((void *) 0);}))
		    if (dev->ifindex == ifindex)
		      return dev;
  return ((void *)0);
}

__attribute__ ((noinline, noclone))
int foo(struct __net *net)
{
  if (!__dev_get_by_index(net, net->ifindex));
    return 1;
  return 0;
}

int main()
{
  return 0;
}

/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
