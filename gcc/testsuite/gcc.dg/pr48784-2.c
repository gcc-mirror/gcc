/* { dg-do run } */
/* { dg-require-effective-target size32plus } */
/* { dg-options "-fno-strict-volatile-bitfields" } */

extern void abort (void);

#pragma pack(1)
volatile struct S0 {
   signed a : 7;
   unsigned b : 28;  /* b can't be fetched with an aligned 32-bit access, */
                     /* but it certainly can be fetched with an unaligned access */
} g = {0,0xfffffff};

int main() {
  unsigned b = g.b;
  if (b != 0xfffffff)
    abort ();
  return 0;
}
