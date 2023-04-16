/* { dg-do compile } */
/* { dg-options "-Os -mmcu=atmega8" } */
int wmul (char a, char b)
{
    return a * (char) (b << 3);
}

/* { dg-final { scan-assembler-times "lsl" 3 } } */
/* { dg-final { scan-assembler-times "muls" 1 } } */
