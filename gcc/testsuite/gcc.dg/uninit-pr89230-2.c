/* PR middle-end/89230 - Bogus uninited usage warning with printf
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

extern void* memset (void*, int, size_t);
extern int printf (const char*, ...);
extern int rand (void);

struct S
{
  int a;
  int b;
};

struct H
{
  int c;
  int d;
};

void getblk (void* blk)
{
  struct S* s = (struct S*) blk;
  memset (blk, 0, 512);
  s->a = rand () & 1;
}

struct H* gethdr (void* blk)
{
  memset (blk, 0, 512);
  return rand () & 1 ? (struct H*) blk : 0;
}

int main (void)
{
  char blk[512], tmp[512];
  struct S *s = (struct S*) blk;
  struct H *h;

  getblk (blk);

  if (s->a  ||  !(h = gethdr (tmp))  ||  s->a != h->d) {

    printf ("%d\n", s->b);
    if (s->a)
      printf ("s->a = %d\n", s->a);
    else if (!h)
      printf ("!h\n");
    else
      printf ("h->d = %d\n", h->d);
  }
}
