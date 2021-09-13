/* Verify that -Wuninitialized warnings about accesses to objects via
   pointers and offsets mention valid expressions.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;

void sink (int);

/* Verify properly aligned accesses at offsets that are multiples of
   the access size.  */

void test_aligned (void)
{
  char *p1 = (char*)__builtin_malloc (32);
  p1 += sizeof (int32_t);

  int16_t *p2 = (int16_t*)p1;
  sink (p2[1]);               // { dg-warning "'\\(\\(int16_t\\*\\)p1\\)\\\[3]' is used uninitialized" }

  int32_t *p4 = (int32_t*)p1;
  sink (p4[1]);               // { dg-warning "'\\(\\(int32_t\\*\\)p1\\)\\\[2]' is used uninitialized" }
}


/* Verify misaligned accesses at offsets that aren't multiples of
   the access size.  */

void test_misaligned (void)
{
  char *p1 = (char*)__builtin_malloc (32);
  p1 += 1;

  int16_t *p2 = (int16_t*)p1;
  sink (p2[1]);               // { dg-warning "'\\(\\(int16_t\\*\\)\\(p1 \\+ 1\\)\\)\\\[1]' is used uninitialized" }

  int32_t *p4 = (int32_t*)p1;
  sink (p4[1]);               // { dg-warning "'\\(\\(int32_t\\*\\)\\(p1 \\+ 1\\)\\)\\\[1]' is used uninitialized" }
}
