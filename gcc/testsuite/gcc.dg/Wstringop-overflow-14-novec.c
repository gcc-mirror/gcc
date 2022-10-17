/* Test to verify that past-the-end multibyte writes via lvalues of wider
   types than char are diagnosed.
   { dg-do compile }
   { dg-require-effective-target int32plus }
   { dg-options "-O2 -fno-tree-vectorize -Wall -Wno-array-bounds" }  */

typedef __INT16_TYPE__  int16_t;

char a4[4], a8[8], a16[16];

void test_int16 (void)
{
  char *p = a4 + 1;
  *(int16_t*)p = 0;
  *(int16_t*)(p + 2) = 0;   // { dg-warning "writing 2 bytes into a region of size 1" }
}
