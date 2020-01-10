/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

struct foo;

void
f1 (svbool_t pg, svint8_t s8, int8_t *s8_ptr, uint8_t *u8_ptr,
    uint16_t *u16_ptr, svbool_t *pg_ptr, const struct foo *foo_ptr)
{
  svwhilerw (u8_ptr); /* { dg-error {too few arguments to function 'svwhilerw'} } */
  svwhilerw (u8_ptr, u8_ptr, u8_ptr); /* { dg-error {too many arguments to function 'svwhilerw'} } */
  svwhilerw (pg, u8_ptr); /* { dg-error {passing 'svbool_t' to argument 1 of 'svwhilerw', which expects a pointer type} } */
  svwhilerw (0, u8_ptr); /* { dg-error {passing 'int' to argument 1 of 'svwhilerw', which expects a pointer type} } */
  svwhilerw (s8, u8_ptr); /* { dg-error {passing 'svint8_t' to argument 1 of 'svwhilerw', which expects a pointer type} } */
  svwhilerw (1, u8_ptr); /* { dg-error {passing 'int' to argument 1 of 'svwhilerw', which expects a pointer type} } */
  svwhilerw (pg_ptr, u8_ptr); /* { dg-error {passing 'svbool_t \*' to argument 1 of 'svwhilerw', but 'svbool_t' is not a valid SVE element type} } */
  svwhilerw (foo_ptr, u8_ptr); /* { dg-error {passing 'const struct foo \*' to argument 1 of 'svwhilerw', but 'struct foo' is not a valid SVE element type} } */
  svwhilerw (u8_ptr, 0); /* { dg-error {passing 'int' to argument 2 of 'svwhilerw', which expects a pointer type} } */
  svwhilerw (u8_ptr, s8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svwhilerw', which expects a pointer type} } */
  svwhilerw (u8_ptr, u8_ptr);
  svwhilerw (u8_ptr, s8_ptr); /* { dg-error {passing 'int8_t \*'[^\n]* to argument 2 of 'svwhilerw', but argument 1 had type 'uint8_t \*'} } */
  svwhilerw (u8_ptr, u16_ptr); /* { dg-error {passing 'uint16_t \*'[^\n]* to argument 2 of 'svwhilerw', but argument 1 had type 'uint8_t \*'} } */
  svwhilerw (s8_ptr, u8_ptr); /* { dg-error {passing 'uint8_t \*'[^\n]* to argument 2 of 'svwhilerw', but argument 1 had type 'int8_t \*'} } */
}
