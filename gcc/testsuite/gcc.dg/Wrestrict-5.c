/* Test to verify that valid calls to common restrict-qualified built-in
   functions declared with no prototype are checked for overlap, and that
   invalid calls are ignored.
  { dg-do compile }
  { dg-options "-O2 -Wrestrict" }  */

typedef __SIZE_TYPE__ size_t;

#if __cplusplus
extern "C" {

#define NO_PROTO ...
#else
#define NO_PROTO /* empty */
#endif

void* memcpy ();
char* strncpy ();

#if __cplusplus
}   /* extern "C" */
#endif

void test_memcpy_warn (char *d)
{
  memcpy (d, d + 2, 3);       /* { dg-warning "accessing 3 bytes at offsets 0 and 2 overlaps 1 byte at offset 2" } */
}

void test_memcpy_nowarn (char *d)
{
  memcpy (d, d + 2, "");
}


void test_strncpy_warn (char *d)
{
  strncpy (d + 1, d + 3, 5);  /* { dg-warning "accessing 5 bytes at offsets 1 and 3 overlaps 2 bytes at offset 3" } */
}

void test_strncpy_nowarn (char *d)
{
  strncpy (d + 1, d + 3, "");
}
