struct PMC {
    unsigned flags;
};

typedef struct Pcc_cell
{
    struct PMC *p;
    long bla;
    long type;
} Pcc_cell;

extern void abort ();
extern void Parrot_gc_mark_PMC_alive_fun(int * interp, struct PMC *pmc)
     __attribute__((noinline));

void Parrot_gc_mark_PMC_alive_fun (int * interp, struct PMC *pmc)
{
  abort ();
}

static void mark_cell(int * interp, Pcc_cell *c)
        __attribute__((__nonnull__(1)))
        __attribute__((__nonnull__(2)))
        __attribute__((noinline));

static void
mark_cell(int * interp, Pcc_cell *c)
{
            if (c->type == 4 && c->p
		&& !(c->p->flags & (1<<18)))
	      Parrot_gc_mark_PMC_alive_fun(interp, c->p);
}

void foo(int * interp, Pcc_cell *c);

void
foo(int * interp, Pcc_cell *c)
{
  mark_cell(interp, c);
}

int main()
{
  int i;
  Pcc_cell c;
  c.p = 0;
  c.bla = 42;
  c.type = 4;
  foo (&i, &c);
  return 0;
}
