/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

signed __int128 foo(signed __int128 x) {
    signed __int128 t = x >> 127;
    return ((x^t)>>1)^t;
}

/* { dg-final { scan-assembler-times "xorq" 4 } } */
/* { dg-final { scan-assembler-times "sarq" 2 } } */
