/* { dg-additional-options "-fanalyzer-checker=taint" } */

typedef __SIZE_TYPE__ size_t;

extern size_t fread (void *, size_t, size_t, void *);

int
test_1 (void *fp)
{
  int i;
  fread (&i, sizeof (i), 1, fp);
  return i;  
}
