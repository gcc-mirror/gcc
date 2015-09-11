/* Verify that IPA-CP can clone mark_cell without miscompiling it despite its
   type_attributes.  */
/* { dg-do run } */
/* { dg-options "-O3 -fdump-ipa-cp" } */


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

extern void abort ();
extern void never_ever(int * interp, struct PMC *pmc)
     __attribute__((noinline));

void never_ever (int * interp, struct PMC *pmc)
{
  abort ();
}

static void mark_cell(int * interp, Pcc_cell *c)
        __attribute__((__nonnull__(1)))
        __attribute__((noinline));

static void
mark_cell(int * interp, Pcc_cell *c)
{
  if (c && c->type == 4 && c->p
      && !(c->p->flags & (1<<14)))
    never_ever(interp, c->p);
}

static void foo(int * interp, Pcc_cell *c)
  __attribute__((noinline));

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

  for (i = 0; i < 100; i++)
    foo (&gi, getnull ());
  return 0;
}


/* { dg-final { scan-ipa-dump "Creating a specialized node of mark_cell" "cp" } } */

