/* Test to verify that past-the-end multibyte writes via lvalues of wider
   types than char are diagnosed.
   { dg-do compile }
   { dg-require-effective-target int32plus }
   { dg-options "-O2 -Wall -Wno-array-bounds" } */

typedef __INT16_TYPE__  int16_t;
typedef __INT32_TYPE__  int32_t;
typedef __INT64_TYPE__  int64_t;
typedef __SIZE_TYPE__   size_t;

void* memcpy (void*, const void*, size_t);
char* strcpy (char*, const char*);

char a4[4], a8[8], a16[16];

const char s4[] = "1234";
const char t4[] = "4321";

void test_memcpy_cond (int i)
{
  char *p = a4 + 1;
  const char *q = i ? s4 : t4;
  // On strictly aligned target the call below is left unchanged and
  // triggers (inaccurately) a -Warray-bounds.  The test suppresses
  // the warning above, which then lets -Wstringop-overrflow detect
  // the overflow just before expansion.
  // On other targets it's transformed into a store of a 4-byte integer
  // which is detected by -Wstringop-overrflow in the strlen pass (i.e.,
  // before it gets to expansion).
  memcpy (p, q, 4);         // { dg-warning "writing 4 bytes into a region of size 3" }
}


void test_int16 (void)
{
  char *p = a4 + 1;
  *(int16_t*)p = 0;
  *(int16_t*)(p + 2) = 0;   // { dg-warning "writing 2 bytes into a region of size 1" }
}


void test_int32 (void)
{
  char *p = a8 + 3;
  *(int32_t*)p = 0;
  *(int32_t*)(p + 2) = 0;   // { dg-warning "writing 4 bytes into a region of size 3" }
}


void test_int64 (void)
{
  char *p = a16 + 5;
  *(int64_t*)p = 0;
  *(int64_t*)(p + 5) = 0;   // { dg-warning "writing 8 bytes into a region of size 6" }
}
