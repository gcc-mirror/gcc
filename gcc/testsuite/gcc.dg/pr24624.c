/* This used to ICE due to a backend problem on s390.  */

/* { dg-do compile } */
/* { dg-options "-O1 -mpacked-stack" } */

typedef unsigned int __u32;
typedef struct
{
  volatile int counter;
} __attribute__ ((aligned (4))) atomic_t;
static __inline__ __attribute__ ((always_inline))
     void atomic_inc (volatile atomic_t * v)
{
  (
    {
    typeof (v->counter) old_val, new_val;
  __asm__ __volatile__ (
	"   l     %0,0(%3)\n" 
	"0: lr    %1,%0\n" 
	"   ar    %1,%4\n" 
	"   cs    %0,%1,0(%3)\n" 
	"   jl    0b": 
	"=&d" (old_val), "=&d" (new_val), "=m" (((atomic_t *) (v))->counter): 
	"a" (v), "d" (1), "m" (((atomic_t *) (v))->counter):
	"cc", "memory");
    });
}
extern unsigned long volatile __attribute__ ((section (".data"))) jiffies;
struct inet_peer
{
  unsigned long dtime;
  atomic_t refcnt;
};
static volatile int peer_total;
int inet_peer_threshold = 65536 + 128;
int inet_peer_minttl = 120 * 100;
int inet_peer_maxttl = 10 * 60 * 100;
static int
cleanup_once (unsigned long ttl)
{
  struct inet_peer *p;
  if (p != ((void *) 0))
    {
      if (((
	     {
	     1;}
	   ) && ((long) (jiffies) - (long) (p->dtime + ttl) < 0)))
	{
	  return -1;
	}
      atomic_inc (&p->refcnt);
    }
}
struct inet_peer *
inet_getpeer (__u32 daddr, int create)
{
  int i;
  int ttl;
  if (peer_total >= inet_peer_threshold)
    ttl = inet_peer_minttl;
  else
    ttl =
      inet_peer_maxttl - (inet_peer_maxttl -
			  inet_peer_minttl) / 100 * peer_total /
      inet_peer_threshold * 100;
  for (i = 0; i < 30 && !cleanup_once (ttl); i++);
}
