/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef struct LINK link;
struct LINK
{
    link* next;
};

int haha(link* p1, link* p2)
{
  if ((p1->next && !p2->next) || p2->next)
    return 0;

  return 1;
}

/* { dg-final { scan-assembler-times "test|cmp" 2 } } */
