/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

typedef struct {
    int alg;
    int h1[8];
    unsigned d1[1];
} tmp;
typedef struct {
    tmp itmp;
    tmp otmp;
} h1;
h1 c;

static void
fn1(char *p1, int p2)
{
  int i = 0;
  for (; i < 4; i++)
    *p1++ = p2;
}

static void
fn2(tmp *p1)
{
  char *d = (char *)p1->d1;
  int *b = p1->h1;
  for (int a = 0; a; a++, d += 4)
    fn1(d, *b++);
}

void fn3() { fn2(&(&c)->otmp); }
