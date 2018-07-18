/* { dg-do run } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

extern void abort (void);

struct S1 {
  unsigned int flag : 1;
  unsigned int size : 31;
};

__attribute__((noipa))
void foo1 (struct S1 *s, unsigned int size)
{
  s->flag = 1;
  s->size = size & 0x7FFFFFFF;
}

struct S2 {
  unsigned int flag : 1;
  unsigned int size : 15;
  unsigned short count;
};

__attribute__((noipa))
void foo2 (struct S2 *s, unsigned short size)
{
  s->flag = 1;
  s->size = size;
  s->count = 0xABCD;
}

struct S3 {
  unsigned int n1 : 4;
  unsigned int c  : 8;
  unsigned int n2 : 4;
};

__attribute__((noipa))
void foo3 (struct S3 *s, unsigned char n1, unsigned char c, unsigned char n2)
{
  s->n1 = n1 & 0xF;
  s->n2 = n2 & 0xF;
  s->c = c;
}

int main (void)
{
  struct S1 s1;
  struct S2 s2;
  struct S3 s3;

  foo1 (&s1, 0x12345678);
  if (s1.flag != 1 || s1.size != 0x12345678)
    abort ();

  foo2 (&s2, 0x1234);
  if (s2.flag != 1 || s2.size != 0x1234 || s2.count != 0xABCD)
    abort ();

  foo3 (&s3, 0x12, 0x34, 0x56);
  if (s3.n1 != 0x2 || s3.c != 0x34 || s3.n2 != 0x6)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 3 "store-merging" } } */
