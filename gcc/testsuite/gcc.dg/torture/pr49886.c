/* { dg-do run } */
/* { dg-xfail-run-if "" { "*-*-*" } { "-O2" "-O3" "-Os" } } */

struct PMC {
    unsigned flags;
};

typedef struct Pcc_cell
{
    struct PMC *p;
    long bla;
    long type;
} Pcc_cell;

int gi;
int cond;

extern void abort ();
extern void never_ever(int interp, struct PMC *pmc)
  __attribute__((noinline,noclone));

void never_ever (int interp, struct PMC *pmc)
{
  abort ();
}

static void mark_cell(int * interp, Pcc_cell *c)
  __attribute__((__nonnull__(1)));

static void
mark_cell(int * interp, Pcc_cell *c)
{
  if (!cond)
    return;

  if (c && c->type == 4 && c->p
      && !(c->p->flags & (1<<18)))
    never_ever(gi + 1, c->p);
  if (c && c->type == 4 && c->p
      && !(c->p->flags & (1<<17)))
    never_ever(gi + 2, c->p);
  if (c && c->type == 4 && c->p
      && !(c->p->flags & (1<<16)))
    never_ever(gi + 3, c->p);
  if (c && c->type == 4 && c->p
      && !(c->p->flags & (1<<15)))
    never_ever(gi + 4, c->p);
  if (c && c->type == 4 && c->p
      && !(c->p->flags & (1<<14)))
    never_ever(gi + 5, c->p);
  if (c && c->type == 4 && c->p
      && !(c->p->flags & (1<<13)))
    never_ever(gi + 6, c->p);
  if (c && c->type == 4 && c->p
      && !(c->p->flags & (1<<12)))
    never_ever(gi + 7, c->p);
  if (c && c->type == 4 && c->p
      && !(c->p->flags & (1<<11)))
    never_ever(gi + 8, c->p);
  if (c && c->type == 4 && c->p
      && !(c->p->flags & (1<<10)))
    never_ever(gi + 9, c->p);
}

static void
foo(int * interp, Pcc_cell *c)
{
  mark_cell(interp, c);
}

static struct Pcc_cell *
__attribute__((noinline,noclone))
getnull(void)
{
  return (struct Pcc_cell *) 0;
}


int main()
{
  int i;

  cond = 1;
  for (i = 0; i < 100; i++)
    foo (&gi, getnull ());
  return 0;
}


void
bar_1 (int * interp, Pcc_cell *c)
{
  c->bla += 1;
  mark_cell(interp, c);
}

void
bar_2 (int * interp, Pcc_cell *c)
{
  c->bla += 2;
  mark_cell(interp, c);
}

