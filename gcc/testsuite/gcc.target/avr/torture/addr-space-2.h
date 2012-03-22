extern void exit (int);
extern void abort (void);

typedef struct T
{
  char val;
  const __as struct T *l, *r;
} tree;

/*
                    abcd   
                   /    \
                 ab      cd
                /  \    /  \
               a    b  c    d
*/

const __as tree a = { 'a', 0, 0 };
const __as tree b = { 'b', 0, 0 };
const __as tree c = { 'c', 0, 0 };
const __as tree d = { 'd', 0, 0 };

const __as tree ab = { 'A', &a, &b };
const __as tree cd = { 'C', &c, &d };

const __as tree abcd = { '*', &ab, &cd };

static void
test1 (void)
{
  if (abcd.val != '*')
    abort();

  if (abcd.l->val != 'A')
    abort();
  if (abcd.r->val != 'C')
    abort();

  if (abcd.l->l->val != 'a')
    abort();
  if (abcd.l->r->val != 'b')
    abort();
  if (abcd.r->l->val != 'c')
    abort();
  if (abcd.r->r->val != 'd')
    abort();
}

static void
test2 (const __as tree *t)
{
  if (t->val != '*')
    abort();

  if (t->l->val != 'A')
    abort();
  if (t->r->val != 'C')
    abort();

  if (t->l->l->val != 'a')
    abort();
  if (t->l->r->val != 'b')
    abort();
  if (t->r->l->val != 'c')
    abort();
  if (t->r->r->val != 'd')
    abort();
}

static void
test3 (const __as tree *pt)
{
  tree t = *pt;
  
  if (t.val != '*')
    abort();

  if (t.l->val != 'A')
    abort();
  if (t.r->val != 'C')
    abort();

  if (t.l->l->val != 'a')
    abort();
  if (t.l->r->val != 'b')
    abort();
  if (t.r->l->val != 'c')
    abort();
  if (t.r->r->val != 'd')
    abort();
}

int main (void)
{
  const __as tree *t = &abcd;
  test1();
  test2 (&abcd);
  test3 (&abcd);

  __asm ("" : "+r" (t));
  test2 (t);
  test3 (t);
  
  exit (0);
  return 0;
}
