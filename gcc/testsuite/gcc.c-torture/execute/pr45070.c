/* PR45070 */
extern void abort(void);

struct packed_ushort {
    unsigned short ucs;
} __attribute__((packed));

struct source {
    int pos, length;
    int flag;
};

static void __attribute__((noinline)) fetch(struct source *p)
{
    p->length = 128;
}
    
static struct packed_ushort __attribute__((noinline)) next(struct source *p)
{
    struct packed_ushort rv;

    if (p->pos >= p->length) {
	if (p->flag) {
	    p->flag = 0;
	    fetch(p);
	    return next(p);
	}
	p->flag = 1;
	rv.ucs = 0xffff;
	return rv;
    }
    rv.ucs = 0;
    return rv;
}

int main(void)
{
    struct source s;
    int i;

    s.pos = 0;
    s.length = 0;
    s.flag = 0;

    for (i = 0; i < 16; i++) {
	struct packed_ushort rv = next(&s);
	if ((i == 0 && rv.ucs != 0xffff)
	    || (i > 0 && rv.ucs != 0))
	    abort();
    }
    return 0;
}
