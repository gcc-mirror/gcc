/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1 -ffinite-loops" } */

typedef struct list {
    char pad[15];
    struct list *next;
} list;

int data;

list *head, *tail;

int __attribute__((pure)) pfn (int);

int foo (unsigned u, int s)
{
  unsigned i;
  list *p;
  int j;

  for (i = 0; i < u; i += 2)
    ;

  for (p = head; p; p = p->next)
    ;

  for (j = data; j & s; j = pfn (j + 3))
    ;

  for (p = head; p != tail; p = p->next)
    for (j = data + 1; j > s; j = pfn (j + 2))
      ;

  return 0;
}
/* { dg-final { scan-tree-dump-not "if" "cddce1"} } */

