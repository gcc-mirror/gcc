/* { dg-do compile } */
/* { dg-options "-Os -mmcu=avrxmega3" } */

unsigned short foo(unsigned char a, unsigned short b) {
    return (unsigned char)((b >> 8) + 0) * a ;
}

/* { dg-final { scan-assembler-times "mul" 1 } } */
/* { dg-final { scan-assembler-times "mov" 1 } } */
/* { dg-final { scan-assembler-not "add" } } */
/* { dg-final { scan-assembler-not "ldi" } } */
