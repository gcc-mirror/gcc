/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* If the constant operand is a single bit and we know the bit is off in
   the other operand, then we can use bseti to turn on the bit.  This
   wins if the constant does not fit into a simm12 for addi.  */

typedef unsigned long u64;

u64
f1 (u64 x)
{
  return (x >> 49) + 0x8000;
}

u64
f2 (u64 x)
{
  return (x >> 40) + (1UL << 24);
}

u64
f3 (u64 x)
{
  return (x >> 33) + (1UL << 31);
}

u64
f4 (u64 x)
{
  return (x >> 20) + (1UL << 44);
}

u64
f5 (u64 x)
{
  return (x >> 1) + (1UL << 63);
}

u64
f6 (u64 x)
{
  return (x & 0x7fff) + 0x8000;
}

u64
f7 (u64 x, u64 y)
{
  return (y << 16) + (x & 0x7fff) + 0x8000;
}

/* The exact shape form from x264  */
unsigned int
f8 (int x, int y)
{
  return ((x << 16) | (y & 0x7FFF)) | 0x8000;
}

/* The constant overlaps the bits the shift can produce: a real add.  */
u64
n1 (u64 x)
{
  return (x >> 48) + 0x8000;
}

/* An arithmetic shift can produce sign bits.  */
long
n2 (long x)
{
  return (x >> 49) + 0x8000;
}

/* The constant fits an addi, which may compress; bseti would be larger.  */
u64
n3 (u64 x)
{
  return (x >> 63) + 0x400;
}

/* f1, f6, f7 and f8 each set bit 15; n1 and n2 must not, as their adds can
   carry.  */
/* { dg-final { scan-assembler-times "bseti\t\[axt\]\[0-9\],\[axt\]\[0-9\],15" 4 } } */
/* n3 keeps its addi, so there must be no bseti of bit 10.  */
/* { dg-final { scan-assembler-not "bseti\t\[axt\]\[0-9\],\[axt\]\[0-9\],10" } } */
/* f2, f3 and f4 set bits 24, 31 and 44.  */
/* { dg-final { scan-assembler-times "bseti\t\[axt\]\[0-9\],\[axt\]\[0-9\],24" 1 } } */
/* { dg-final { scan-assembler-times "bseti\t\[axt\]\[0-9\],\[axt\]\[0-9\],31" 1 } } */
/* { dg-final { scan-assembler-times "bseti\t\[axt\]\[0-9\],\[axt\]\[0-9\],44" 1 } } */
/* f5 sets bit 63.  After the shift that bit is clear, so bseti and binvi are
   equivalent.  */
/* { dg-final { scan-assembler-times "(?:bseti|binvi)\t\[axt\]\[0-9\],\[axt\]\[0-9\],63" 1 } } */
