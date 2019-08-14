/* { dg-do compile } */
/* { dg-options "-O2 -mstv" } */

#define max(a,b) (((a) > (b))? (a) : (b))
#define min(a,b) (((a) < (b))? (a) : (b))

int ssi[1024];
unsigned int usi[1024];
long long sdi[1024];
unsigned long long udi[1024];

#define CHECK(FN, VARIANT) \
void \
FN ## VARIANT (void) \
{ \
  for (int i = 1; i < 1024; ++i) \
    VARIANT[i] = FN(VARIANT[i-1], VARIANT[i]); \
}

CHECK(max, ssi);
CHECK(min, ssi);
CHECK(max, usi);
CHECK(min, usi);
CHECK(max, sdi);
CHECK(min, sdi);
CHECK(max, udi);
CHECK(min, udi);
