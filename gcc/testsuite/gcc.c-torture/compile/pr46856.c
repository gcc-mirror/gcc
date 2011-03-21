struct data {
    int prio;
    signed char status;
};

struct base {
    unsigned _num;
    struct data vec[10];
};

static struct data *ix(struct base *base, unsigned i)
{
    return &base->vec[i];
}

struct heap {
    struct base base;
};

struct heap *heap;

void increase_insn_priority (int *fld, int amount)
{
    if (ix(heap ? &heap->base : 0, *fld)->status > 0)
	ix(heap ? &heap->base : 0, *fld)->prio += amount;
}
