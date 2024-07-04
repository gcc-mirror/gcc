/* testing the correct usage of flexible array members in unions 
   and alone in structures: initialization  */
/* { dg-do run} */
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
  char a[];  
  int b[];  
} with_fam_3_v = {.b = {0x1f2f3f4f, 0x5f6f7f7f}};

struct only_fam {
  int b[]; 
} only_fam_v = {{7, 11}};

struct only_fam_2 {
  unsigned int : 2;
  unsigned int : 3;
  int b[]; 
} only_fam_2_v = {{7, 11}};

int main ()
{
  if (with_fam_1_v.b[3] != 4
      || with_fam_1_v.b[0] != 1)
    __builtin_abort ();
  if (with_fam_2_v.b[3] != 0x1f
      || with_fam_2_v.b[0] != 0x4f)
    __builtin_abort ();
  if (with_fam_3_v.a[0] != 0x4f
      || with_fam_3_v.a[7] != 0x5f)
    __builtin_abort ();
  if (only_fam_v.b[0] != 7
      || only_fam_v.b[1] != 11)
    __builtin_abort ();
  if (only_fam_2_v.b[0] != 7
      || only_fam_2_v.b[1] != 11)
    __builtin_abort ();

  return 0;
}

