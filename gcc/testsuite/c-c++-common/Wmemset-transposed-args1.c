/* { dg-do compile } */
/* { dg-options "-Wall" } */

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
  memset (buf, sizeof buf, 0);	/* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, '\0'); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, L'\0'); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, 1, 1 - 1);
  memset (buf, 1, 0 - 0);
  memset (buf, 0, 0);
  memset (buf, '\0', 0);
  memset (buf, L'\0', 0);
  memset (buf, 1 - 1, 0); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, 0 - 0, 0); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, 0L);	/* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, 0UL); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, 0LL); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, 0ULL); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, (int) 0);
  memset (buf, sizeof buf, -0);
}
