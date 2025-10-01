/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-fdump-tree-original-all" } */

typedef int v4i __attribute__((vector_size(4*sizeof(int))));

/* Ensure we can simplify `VEC_COND_EXPR(a OP1 b) OP2 VEC_COND_EXPR(a OP3 b)`
 * into `VEC_COND_EXPR(a OP4 b)`
 */

void use (v4i const *z);

void
g (v4i *x, v4i const *y, v4i *z, v4i *t)
{
  *z = *x > *y | *x == *y; // expect >=
  *t = *x > *y | *x <= *y; // expect true
}

void
h (v4i *x, v4i const *y, v4i *z, v4i *t)
{
  *z = *x <= *y & *x >= *y; // expect x == y
  *t = *x <= *y & *x != *y; // expect x<y
}

void
i (v4i *x, v4i const *y, v4i *z, v4i *t)
{
  *z = *x == *y | *x != *y; // expect true
  *t = *x == *y & *x != *y; // expect false
}

void
k (v4i *x, v4i const *y, v4i *z, v4i *t)
{
  *z = *x < *y | *x == *y;  // x <= y
  *t = *x < *y & *x > *y;   // expect false
}

void
m (v4i *x, v4i const *y, v4i *z, v4i *t)
{
  *z = *x <= *y ^ *x >= *y; /* expect x != y */
  *t = *x <= *y ^ *x != *y; /* expect x <= y */
}

void
n (v4i *x, v4i const *y, v4i *z, v4i *t)
{
  *z = *x == *y ^ *x != *y; /* expect true */
  *t = *x == *y ^ *x == *y; /* expect false */
}


/* { dg-final { scan-tree-dump-times "\\s*\\*tD\\.\\d+\\s*=\\s*\\{\\s*-1(?:,\\s*-1){3}\\s*\\}\\s*;" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "\\s*\\*tD\\.\\d+\\s*=\\s*\\{\\s*0(?:,\\s*0){3}\\s*\\}\\s*;" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "\\s*\\*zD\\.\\d+\\s*=\\s*\\{\\s*-1(?:,\\s*-1){3}\\s*\\}\\s*;" 2 "original" } } */

/* { dg-final { scan-tree-dump ".*\\*zD\\.\\d+\\s*=\\s*VEC_COND_EXPR\\s*<\\s*\\*xD\\.\\d+\\s*>=\\s*VIEW_CONVERT_EXPR<v4iD\\.\\d+>\\(\\*yD\\.\\d+\\)\\s*,\\s*\\{\\s*-1(,\\s*-1){3}\\s*\\}\\s*,\\s*\\{\\s*0(,\\s*0){3}\\s*\\}\\s*>\\s*;" "original" } } */
/* { dg-final { scan-tree-dump ".*\\*zD\\.\\d+\\s*=\\s*VEC_COND_EXPR\\s*<\\s*\\*xD\\.\\d+\\s*==\\s*VIEW_CONVERT_EXPR<v4iD\\.\\d+>\\(\\*yD\\.\\d+\\)\\s*,\\s*\\{\\s*-1(,\\s*-1){3}\\s*\\}\\s*,\\s*\\{\\s*0(,\\s*0){3}\\s*\\}\\s*>\\s*;" "original" } } */
/* { dg-final { scan-tree-dump ".*\\*tD\\.\\d+\\s*=\\s*VEC_COND_EXPR\\s*<\\s*\\*xD\\.\\d+\\s*<\\s*VIEW_CONVERT_EXPR<v4iD\\.\\d+>\\(\\*yD\\.\\d+\\)\\s*,\\s*\\{\\s*-1(,\\s*-1){3}\\s*\\}\\s*,\\s*\\{\\s*0(,\\s*0){3}\\s*\\}\\s*>\\s*;" "original" } } */
/* { dg-final { scan-tree-dump ".*\\*zD\\.\\d+\\s*=\\s*VEC_COND_EXPR\\s*<\\s*\\*xD\\.\\d+\\s*<=\\s*VIEW_CONVERT_EXPR<v4iD\\.\\d+>\\(\\*yD\\.\\d+\\)\\s*,\\s*\\{\\s*-1(,\\s*-1){3}\\s*\\}\\s*,\\s*\\{\\s*0(,\\s*0){3}\\s*\\}\\s*>\\s*;" "original" } } */
/* { dg-final { scan-tree-dump ".*\\*zD\\.\\d+\\s*=\\s*VEC_COND_EXPR\\s*<\\s*\\*xD\\.\\d+\\s*!=\\s*VIEW_CONVERT_EXPR<v4iD\\.\\d+>\\(\\*yD\\.\\d+\\)\\s*,\\s*\\{\\s*-1(,\\s*-1){3}\\s*\\}\\s*,\\s*\\{\\s*0(,\\s*0){3}\\s*\\}\\s*>\\s*;" "original" } } */
/* { dg-final { scan-tree-dump ".*\\*tD\\.\\d+\\s*=\\s*VEC_COND_EXPR\\s*<\\s*\\*xD\\.\\d+\\s*>=\\s*VIEW_CONVERT_EXPR<v4iD\\.\\d+>\\(\\*yD\\.\\d+\\)\\s*,\\s*\\{\\s*-1(,\\s*-1){3}\\s*\\}\\s*,\\s*\\{\\s*0(,\\s*0){3}\\s*\\}\\s*>\\s*;" "original" } } */
