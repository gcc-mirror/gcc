/* { dg-require-alias "" } */
typedef unsigned char __u8;
typedef __u8 u8;
typedef u8 u_int8_t;
typedef unsigned int gfp_t;

struct list_head
{
  struct list_head *next, *prev;
};
extern int strcmp (const char *, const char *);
enum
{
  NFPROTO_UNSPEC = 0,
  NFPROTO_INET = 1,
  NFPROTO_IPV4 = 2,
  NFPROTO_ARP = 3,
  NFPROTO_NETDEV = 5,
  NFPROTO_BRIDGE = 7,
  NFPROTO_IPV6 = 10,
  NFPROTO_DECNET = 12,
  NFPROTO_NUMPROTO,
};

struct xt_target
{
  struct list_head list;
  const char name[29];
  u_int8_t revision;
};

struct xt_af
{
  struct list_head target;
};

static struct xt_af *xt;

struct xt_af * kcalloc (int, int, int);

static int
target_revfn (u8 af, const char *name, u8 revision, int *bestp)
{
  const struct xt_target *t;
  int have_rev = 0;

  for (t = (
	     {
	     void *__mptr = (void *)((&xt[af].target)->next);
	     ((typeof (*t) *) (__mptr -
			       __builtin_offsetof (typeof (*t), list)));}
       ); &t->list != (&xt[af].target); t = (
					      {
					      void *__mptr =
					      (void *)((t)->list.next);
					      ((typeof (*(t)) *) (__mptr -
								  __builtin_offsetof
								  (typeof
								   (*(t)),
								   list)));}
       ))
    {
      if (strcmp (t->name, name) == 0)
	{
	  if (t->revision > *bestp)
	    *bestp = t->revision;
	  if (t->revision == revision)
	    have_rev = 1;
	}
    }

  if (af != NFPROTO_UNSPEC && !have_rev)
    return target_revfn (NFPROTO_UNSPEC, name, revision, bestp);

  return have_rev;
}

int
xt_find_revision (u8 af, const char *name, u8 revision, int target, int *err)
{
  int have_rev, best = -1;

  have_rev = target_revfn (af, name, revision, &best);


  if (best == -1)
    {
      *err = -2;
      return 0;
    }

}


static int __attribute__ ((__section__ (".init.text")))
  __attribute__ ((__cold__)) xt_init (void)
{
  xt =
    kcalloc (NFPROTO_NUMPROTO, sizeof (struct xt_af),
	     (((gfp_t) (0x400u | 0x800u)) | ((gfp_t) 0x40u) |
	      ((gfp_t) 0x80u)));
}

int init_module (void) __attribute__ ((__copy__ (xt_init)))
  __attribute__ ((alias ("xt_init")));;
