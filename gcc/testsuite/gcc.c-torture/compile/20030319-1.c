/* PR 10073 */
typedef struct
{
  unsigned short digits[4];
} INT_64;

INT_64 int_64_com (INT_64 a)
{
  a.digits[0] ^= 0xFFFF;
  a.digits[1] ^= 0xFFFF;
  a.digits[2] ^= 0xFFFF;
  a.digits[3] ^= 0xFFFF;
  return a;
}
