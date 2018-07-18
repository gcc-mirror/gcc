/* { dg-do link } */
/* { dg-options "-O -fdump-tree-fre1-details" } */

void link_error (void);

void test1 (int *p, int *q)
{
  *p = 1;
  *q = 1;
  if (*p != 1)
    link_error ();
}

void test2 (int *p, int *q, int t)
{
  *p = t;
  *q = t;
  if (*p != t)
    link_error ();
}

void test3 (int *q, int *p)
{
  int tem = *p;
  *q = tem;
  if (*p != tem)
    link_error ();
}

char a[4];
struct A { char a[4]; };
void test4 (struct A *p)
{
  a[0] = p->a[0];
  a[0] = p->a[0];
  a[0] = p->a[0];
}

int main() { return 0; }

/* { dg-final { scan-tree-dump-times "Replaced \\\*p" 3 "fre1" } } */
/* { dg-final { scan-tree-dump-times "Replaced p_.\\(D\\)->" 2 "fre1" } } */
/* { dg-final { scan-tree-dump-times "Deleted redundant store a\\\[0\\\]" 2 "fre1" } } */
