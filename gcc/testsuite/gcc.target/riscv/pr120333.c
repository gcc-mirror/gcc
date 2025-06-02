/* { dg-do run { target { rv64 } } } */
/* { dg-additional-options "-march=rv64gcb -std=gnu23" } */

__attribute__ ((noipa)) _Bool
foo (unsigned char ch, unsigned long mask) {
  return (mask << (0x3f - (ch & 0x3f))) >> 0x3f;
}

int main()
{
  if (!foo (0x3f, 0x8000000000000000ul))
    __builtin_abort ();
  return 0;
}

