/* { dg-options "-mcompact-branches=never -mno-abicalls -G4" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" "-Os" } { "" } } */
/* { dg-final { scan-assembler "beq.*\n\tlw" } } */
/* { dg-final { scan-assembler-times "\\(foo\\)" 2 } } */

/* Test that when compact branches are explicitly disabled, that a non-compact
   branch is produced. 'foo' should be referenced twice in the program text as the
   eager delay slot filler will duplicate the load of foo. */

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
