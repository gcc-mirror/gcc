/* { dg-do compile } */
/* { dg-options "-O0 -fno-math-errno -fdump-rtl-expand" } */

extern float sqrtf(float);

float a(float x)
{
    return sqrtf(x);
}
float b(float x)
{
    return sqrtf(x);
}
/* Here the calls to sqrtf should be expanded into CALL_INSNs, rather than
   fpu sqrtf rtl patterns.  */
/* { dg-final { scan-rtl-dump-times "call_insn" 2 "expand" } } */
/* { dg-final { cleanup-rtl-dump "expand" } } */
