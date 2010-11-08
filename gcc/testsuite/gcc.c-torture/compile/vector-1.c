/* { dg-options "-msse" { target { i?86-*-* x86_64-*-* } } } */
#define vector __attribute__((vector_size(16) ))
struct ss
{
 vector float mVec;
};
float getCapsule(vector int t)
{
 vector float t1 = (vector float)t;
 struct ss y = {t1};
 return *((float*)&y.mVec);
}
