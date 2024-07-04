/* testing flexible array members in unions and alone in structures:
   __bos/__bdos  */
/* { dg-do run } */
/* { dg-options "-O2" } */

union with_fam_1 {
  char a;
  int b[]; 
} with_fam_1_v = {.b = {1, 2, 3, 4, 5}};

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
  if (__builtin_object_size(with_fam_1_v.b, 1) != 20)
    __builtin_abort ();
  if (__builtin_object_size(with_fam_2_v.b, 1) != 4)
    __builtin_abort ();
  if (__builtin_object_size(with_fam_3_v.b, 1) != 8)
    __builtin_abort ();
  if (__builtin_object_size(only_fam_v.b, 1) != 8)
    __builtin_abort ();
  if (__builtin_object_size(only_fam_2_v.b, 1) != 8)
    __builtin_abort ();

  return 0;
}
