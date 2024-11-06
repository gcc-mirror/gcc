/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-std=c23" { target c } } */

static unsigned char a[] = {
#embed __FILE__ limit (125)
};

void
foo (unsigned char *p)
{
  for (int i = 0; i < 128; ++i)
    if (p[i] != ((i < 64 || i == 127) ? (unsigned char) -1 : i - 64 + 33))
      __builtin_abort ();
  if (__builtin_memcmp (p + 128, a, 125))
    __builtin_abort ();
  for (int i = 253; i < 256; ++i)
    if (p[i] != (unsigned char) -1)
      __builtin_abort ();
}

#ifdef __cplusplus
#define M1 (unsigned char) -1
#else
#define M1 -1
#endif

int
main ()
{
  unsigned char res[256] = {
    M1, M1, M1, M1, M1, M1, M1, M1, 
    M1, M1, M1, M1, M1, M1, M1, M1, 
    M1, M1, M1, M1, M1, M1, M1, M1, 
    M1, M1, M1, M1, M1, M1, M1, M1, 
    M1, M1, M1, M1, M1, M1, M1, M1, 
    M1, M1, M1, M1, M1, M1, M1, M1, 
    M1, M1, M1, M1, M1, M1, M1, M1, 
    M1, M1, M1, M1, M1, M1, M1, M1, 
    33, 34, 35, 36, 37, 38, 39, 40, 
    41, 42, 43, 44, 45, 46, 47, 48, 
    49, 50, 51, 52, 53, 54, 55, 56, 
    57, 58, 59, 60, 61, 62, 63, 64, 
    65, 66, 67, 68, 69, 70, 71, 72, 
    73, 74, 75, 76, 77, 78, 79, 80, 
    81, 82, 83, 84, 85, 86, 87, 88, 
    89, 90, 91, 92, 93, 94, 95, M1,
  #embed __FILE__ limit (125) suffix (,)
    M1, M1, M1
  };
  foo (res);
}
