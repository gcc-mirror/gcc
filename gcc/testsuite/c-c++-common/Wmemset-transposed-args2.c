/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-Wall" } */
/* { dg-additional-options "-std=gnu99" { target c } } */

typedef __SIZE_TYPE__ size_t;
extern
#ifdef __cplusplus
"C"
#endif
void *memset (void *, int, size_t);
char buf[1024];

void
foo ()
{
  memset (buf, sizeof buf, u'\0'); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, U'\0'); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, u'\0', 0);
  memset (buf, U'\0', 0);
}
