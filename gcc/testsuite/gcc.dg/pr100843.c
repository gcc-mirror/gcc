/* { dg-do compile } */
/* { dg-options "-O1 -w" } */

char c;
void *memset();
void test_integer_conversion_memset(void *d) {
  memset(d, '\0', c);
}
