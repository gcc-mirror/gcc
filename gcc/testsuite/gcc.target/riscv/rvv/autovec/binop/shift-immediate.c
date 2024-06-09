/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -march=rv32gcv -mabi=ilp32d -O2 -mrvv-vector-bits=scalable" } */

#define uint8_t unsigned char

void foo1 (uint8_t *a)
{
    uint8_t b = a[0];
    int val = 0;

    for (int i = 0; i < 4; i++)
    {
        a[i] = (val & 1) ? (-val) >> 17 : val;
        val += b;
    }
}
