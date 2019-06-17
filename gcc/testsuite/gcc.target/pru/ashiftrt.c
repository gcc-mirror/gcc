/* Test shift optimization */

/* { dg-options "-O1" } */

/* -O1 in the options is significant.  */

extern void func2(unsigned char);

void test(unsigned char v)
{
    /* { dg-final { scan-assembler "lsl\tr14.b0, r14.b0, .\+\n\tlsr\tr14.b0, r14.b0" } } */
    func2((v & 2) ? 1 : 0);
}
