/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int iorhi(unsigned int x) { return (x & 0xffff) | 0x20000; }
unsigned int xorhi(unsigned int x) { return (x & 0xffff) ^ 0x20000; }
unsigned int addhi(unsigned int x) { return (x & 0xffff) + 0x20000; }

unsigned int iorlo(unsigned int x) { return (x & ~0xffff) | 0x02; }
unsigned int xorlo(unsigned int x) { return (x & ~0xffff) ^ 0x02; }
unsigned int addlo(unsigned int x) { return (x & ~0xffff) + 0x02; }

/* { dg-final { scan-assembler-times "movk\t" 6 } } */
