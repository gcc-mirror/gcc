/* testing the correct usage of flexible array members in unions 
   and alone in structures.  */
/* { dg-do run } */
/* { dg-options "-Wpedantic" } */

union with_fam_1 {
  int a;
  int b[];  /* { dg-warning "flexible array member in union is a GCC extension" } */
};

union with_fam_2 {
  char a;
  int b[];  /* { dg-warning "flexible array member in union is a GCC extension" } */
};

union with_fam_3 {
  char a[];  /* { dg-warning "flexible array member in union is a GCC extension" } */
  /* { dg-warning "in an otherwise empty" "" { target c++ } .-1 } */
  int b[];  /* { dg-warning "flexible array member in union is a GCC extension" } */
};

struct only_fam {
  int b[];
  /* { dg-warning "in a struct with no named members" "" { target c } .-1 } */
  /* { dg-warning "in an otherwise empty" "" { target c++ } .-2 } */
  /* { dg-warning "forbids flexible array member" "" { target c++ } .-3 } */
};

struct only_fam_2 {
  unsigned int : 2;
  unsigned int : 3;
  int b[];
  /* { dg-warning "in a struct with no named members" "" { target c } .-1 } */
  /* { dg-warning "in an otherwise empty" "" { target c++ } .-2 } */
  /* { dg-warning "forbids flexible array member" "" { target c++ } .-3 } */
};

int main ()
{
  if (sizeof (union with_fam_1) != sizeof (int))
    __builtin_abort ();
  if (sizeof (union with_fam_2) != __alignof__ (int))
    __builtin_abort ();
  if (sizeof (union with_fam_3) != 0)
    __builtin_abort ();
  if (sizeof (struct only_fam) != 0)
    __builtin_abort ();
  if (sizeof (struct only_fam_2) != __alignof__ (int))
    __builtin_abort ();
  return 0;
}

