/* { dg-do compile } */
/* { dg-require-effective-target lto } */
/* { dg-options "-g -fdebug-types-section -flto -ffat-lto-objects" } */

struct {
  int h;
  unsigned char data[20 + 24 * 6];
} _EC_X9_62_PRIME_192V2;
struct {
  int h;
  unsigned char data[20 + 24 * 6];
} _EC_X9_62_PRIME_192V3;
