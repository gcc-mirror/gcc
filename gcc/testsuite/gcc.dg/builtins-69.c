/* PR middle-end/86308 - ICE in verify_gimple calling index() with
   an invalid declaration
   { dg-do compile }
   { dg-options "-O2 -Wall" }  */

int index (int, int);   /* { dg-warning "conflicting types for built-in function .index.; expected .char \\\*\\\(const char \\\*, int\\\)." } */

int test_index (void)
{
  return index (0, 0);
}


/* PR middle-end/86202 - ICE in get_range_info calling an invalid memcpy()
   declaration */

void *memcpy (void *, void *, __SIZE_TYPE__ *);   /* { dg-warning "conflicting types for built-in function .memcpy.; expected .void \\\*\\\(void \\\*, const void \\\*, \(long \)?unsigned int\\\)." } */

void test_memcpy (void *p, void *q, __SIZE_TYPE__ *r)
{
  memcpy (p, q, r);
}
