/* { dg-do run } */
/* { dg-require-effective-target p9vector_hw } */
/* Force vectorization with -fno-vect-cost-model to have vector unpack
   which exposes the issue in PR115355.  */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -fno-vect-cost-model" } */

/* Verify it runs successfully.  */

__attribute__((noipa))
void setToIdentityGOOD(unsigned long long *mVec, unsigned int mLen)
{
  #pragma GCC novector
  for (unsigned int i = 0; i < mLen; i++)
    mVec[i] = i;
}

__attribute__((noipa))
void setToIdentityBAD(unsigned long long *mVec, unsigned int mLen)
{
  for (unsigned int i = 0; i < mLen; i++)
    mVec[i] = i;
}

unsigned long long vec1[100];
unsigned long long vec2[100];

int main()
{
  unsigned int l = 29;
  setToIdentityGOOD (vec1, 29);
  setToIdentityBAD (vec2, 29);

  if (__builtin_memcmp (vec1, vec2, l * sizeof (vec1[0])) != 0)
    __builtin_abort ();

  return 0;
}
