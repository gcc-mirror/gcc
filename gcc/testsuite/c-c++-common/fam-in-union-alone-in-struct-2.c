/* testing the correct usage of flexible array members in unions 
   and alone in structures: initialization  */
/* { dg-do run } */
/* { dg-options "-O2" } */

union with_fam_1 {
  int a;
  int b[]; 
} with_fam_1_v = {.b = {1, 2, 3, 4}};

union with_fam_2 {
  int a;
  char b[];  
} with_fam_2_v = {.a = 0x1f2f3f4f};

union with_fam_3 {
  unsigned char a[];  
  int b[];  
} with_fam_3_v = {.b = {0x1f2f3f4f, 0x5f6f7f8f}};

struct only_fam {
  int b[]; 
} only_fam_v = {{7, 11}};

struct only_fam_2 {
  unsigned int : 2;
  unsigned int : 3;
  int b[]; 
} only_fam_2_v = {{7, 11}};

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define WITH_FAM_2_V_B0 0x4f
#define WITH_FAM_2_V_B3 0x1f
#define WITH_FAM_3_V_A0 0x4f
#define WITH_FAM_3_V_A7 0x5f
#else
#define WITH_FAM_2_V_B0 0x1f
#define WITH_FAM_2_V_B3 0x4f
#define WITH_FAM_3_V_A0 0x1f
#define WITH_FAM_3_V_A7 0x8f
#endif

int main ()
{
  if (with_fam_1_v.b[3] != 4
      || with_fam_1_v.b[0] != 1)
    __builtin_abort ();
  if (with_fam_2_v.b[3] != WITH_FAM_2_V_B3
      || with_fam_2_v.b[0] != WITH_FAM_2_V_B0)
    __builtin_abort ();
  if (with_fam_3_v.a[0] != WITH_FAM_3_V_A0
      || with_fam_3_v.a[7] != WITH_FAM_3_V_A7)
    __builtin_abort ();
  if (only_fam_v.b[0] != 7
      || only_fam_v.b[1] != 11)
    __builtin_abort ();
  if (only_fam_2_v.b[0] != 7
      || only_fam_2_v.b[1] != 11)
    __builtin_abort ();

  return 0;
}

