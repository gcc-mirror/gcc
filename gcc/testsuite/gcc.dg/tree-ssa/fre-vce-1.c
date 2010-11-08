/* { dg-options "-O2 -fdump-tree-fre -w" } */
/* { dg-options "-O2 -fdump-tree-fre -w -msse" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-do compile } */
#define vector __attribute__((vector_size(sizeof(int)*4) ))
struct s { vector int i; };
vector float f(struct s *sv)
{
  sv->i = (vector int){1, 2, 3, 4};
  return (vector float)sv->i;
}


vector float f1(struct s *sv, vector int a)
{
  sv->i = a;
  return (vector float)sv->i;
}

struct s1 { int i; };

void g(struct s1 *, float);
void a1 (struct s1 sv)
{
  sv.i = 1;
  g(&sv, *(float*)&sv.i);
}


void a2 (struct s1 sv, int i)
{
  sv.i = i;
  g(&sv, *(float*)&sv.i);
}

/* { dg-final { scan-tree-dump-times "sv_\[0-9\]\\\(D\\\)->i" 2 "fre" } } */
/* { dg-final { scan-tree-dump-times "sv.i" 2 "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
