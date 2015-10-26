/* { dg-options "isa_rev>=6 -mcompact-branches=optimal -mno-abicalls -G4" } */
/* { dg-final { scan-assembler-not "bne\t" } } */
/* { dg-final { scan-assembler-not "beq\t" } } */
/* { dg-final { scan-assembler-times "\\(foo\\)" 1 } } */

/* Test that when compact branches are used, that a compact branch is
   produced in the case where code expansion would have occurred if a
   delay slot branch would have be used.  'foo' should only be
   referenced once in the program text.  */

struct list
{
  struct list *next;
  int element;
};

struct list *gr;

int foo;

extern void t (int, int, int*);

void
f (struct list **ptr)
{
  if (gr)
    *ptr = gr->next;
  t (1, foo, &gr->element);
}
