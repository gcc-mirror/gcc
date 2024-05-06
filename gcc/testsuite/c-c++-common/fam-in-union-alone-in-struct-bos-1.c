/* testing flexible array members in unions and alone in structures:
   __bos/__bdos  */
/* { dg-do run } */
/* { dg-options "-O2" } */

union with_fam_1 {
  char a;
  int b[]; 
} *with_fam_1_v;

union with_fam_2 {
  int a;
  char b[];  
} *with_fam_2_v;

union with_fam_3 {
  char a[];  
  int b[];  
} *with_fam_3_v;

struct only_fam {
  int b[]; 
} *only_fam_v;

struct only_fam_2 {
  unsigned int : 2;
  unsigned int : 3;
  int b[]; 
} *only_fam_2_v;

void __attribute__((__noinline__))
setup (int n1, int n2, int n3, int n4, int n5)
{
  with_fam_1_v = (union with_fam_1 *) __builtin_malloc (n1 * sizeof (int));
  with_fam_2_v = (union with_fam_2 *) __builtin_malloc (n2 * sizeof (char));
  with_fam_3_v = (union with_fam_3 *) __builtin_malloc (n3 * sizeof (int));
  only_fam_v = (struct only_fam *) __builtin_malloc (n4 * sizeof (int));
  only_fam_2_v = (struct only_fam_2 *) __builtin_malloc (n5 * sizeof (int));
  return;
}

void __attribute__((__noinline__)) stuff(
    union with_fam_1 *with_fam_1_v,
    union with_fam_2 *with_fam_2_v,
    union with_fam_3 *with_fam_3_v,
    struct only_fam *only_fam_v,
    struct only_fam_2 *only_fam_2_v)
{
  if (__builtin_object_size(with_fam_1_v->b, 1) != -1)
    __builtin_abort (); 
  if (__builtin_object_size(with_fam_2_v->b, 1) != -1)
    __builtin_abort ();
  if (__builtin_object_size(with_fam_3_v->b, 1) != -1)
    __builtin_abort ();
  if (__builtin_object_size(only_fam_v->b, 1) != -1)
    __builtin_abort ();
  if (__builtin_object_size(only_fam_2_v->b, 1) != -1)
    __builtin_abort ();
}

int main (int argc, char *argv[])
{
  setup (2, 3, 4, 5, 6);
  stuff (with_fam_1_v, with_fam_2_v, with_fam_3_v, only_fam_v, only_fam_2_v);
  return 0;
}
