/* PR 5312 
   The problem here is that the ia64 scheduler saw a sequence of L L M type
   insns, and messed up its internal state on which slot it was issuing
   to, and aborted.  */

/* { dg-do compile { target ia64-*-* } } */
/* In ILP32 mode, we get warnings about large integer constants.
   Those cause spurious FAILs.  */
/* { dg-options "-w -O2 -mconstant-gp" } */

typedef unsigned long __u64;
typedef unsigned int __u32;
typedef struct { } spinlock_t;
struct cpuinfo_ia64 {
        union {
                struct {
                        __u32 irq_count;
                        __u32 bh_count;
                } f;
                __u64 irq_and_bh_counts;
        } irq_stat;
        __u32 softirq_pending;
} __attribute__ ((aligned ((1UL << 14)))) ;
enum
{
        TCA_UNSPEC,
        TCA_KIND,
        TCA_OPTIONS,
        TCA_STATS,
        TCA_XSTATS,
        TCA_RATE,
};
struct tc_stats
{
        __u64 bytes;
        __u32 packets;
        __u32 drops;
        __u32 overlimits;
        __u32 bps;
        __u32 pps;
        __u32 qlen;
        __u32 backlog;
        spinlock_t *lock;
};
struct sk_buff {
        unsigned int data_len;
        unsigned char *tail;
        unsigned char *end;
};
static inline int skb_is_nonlinear(const struct sk_buff *skb)
{
        return skb->data_len;
}
static inline int skb_tailroom(const struct sk_buff *skb)
{
        return skb_is_nonlinear(skb) ? 0 : skb->end-skb->tail;
}
struct rtattr
{
        unsigned short rta_len;
        unsigned short rta_type;
};
int qdisc_copy_stats(struct sk_buff *skb, struct tc_stats *st)
{
        do { do { (((struct cpuinfo_ia64 *) (0xa000000000000000 + 2*(1UL << 14)))->irq_stat.f.bh_count)++; __asm__ __volatile__("": : :"memory"); } while (0); (void)(st->lock); } while (0);
        ({ if (skb_tailroom(skb) < (int)( (((( ((sizeof(struct rtattr))+4 -1) & ~(4 -1) ) + ((char*)&st->lock - (char*)st)))+4 -1) & ~(4 -1) )) goto rtattr_failure; __rta_fill(skb, TCA_STATS, (char*)&st->lock - (char*)st, st); });
        do { do { } while(0); do { do { __asm__ __volatile__("": : :"memory"); (((struct cpuinfo_ia64 *) (0xa000000000000000 + 2*(1UL << 14)))->irq_stat.f.bh_count)--; } while (0); if (__builtin_expect((((struct cpuinfo_ia64 *) (0xa000000000000000 + 2*(1UL << 14)))->softirq_pending), 0) && (((struct cpuinfo_ia64 *) (0xa000000000000000 + 2*(1UL << 14)))->irq_stat.f.bh_count) == 0) do_softirq(); } while (0); } while (0);
        return 0;
rtattr_failure:
        do { do { } while(0); do { do { __asm__ __volatile__("": : :"memory"); (((struct cpuinfo_ia64 *) (0xa000000000000000 + 2*(1UL << 14)))->irq_stat.f.bh_count)--; } while (0); if (__builtin_expect((((struct cpuinfo_ia64 *) (0xa000000000000000 + 2*(1UL << 14)))->softirq_pending), 0) && (((struct cpuinfo_ia64 *) (0xa000000000000000 + 2*(1UL << 14)))->irq_stat.f.bh_count) == 0) do_softirq(); } while (0); } while (0);
        return -1;
}
