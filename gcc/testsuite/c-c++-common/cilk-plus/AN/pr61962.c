/* PR other/61962 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

struct FloatStruct
{
    float *f;
};

/* Either SRC or DST must be a struct, otherwise the bug does not occur.  */
void f (struct FloatStruct* dst, float *src, unsigned int length)
{
    dst->f[0:length] = src[0:length];
}
