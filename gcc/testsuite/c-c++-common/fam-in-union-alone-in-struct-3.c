/* testing the correct usage of flexible array members in unions 
   and alone in structures.  */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

union with_fam_1 {
  int a;
  int b[];  /* { dg-error "flexible array member in union is a GCC extension" } */
};

union with_fam_2 {
  char a;
  int b[];  /* { dg-error "flexible array member in union is a GCC extension" } */
};

union with_fam_3 {
  char a[];  /* { dg-error "flexible array member in union is a GCC extension" } */
  /* { dg-error "in an otherwise empty" "" { target c++ } .-1 } */
  int b[];  /* { dg-error "flexible array member in union is a GCC extension" } */
};

struct only_fam {
  int b[];
  /* { dg-error "in a struct with no named members" "" { target c } .-1 } */
  /* { dg-error "in an otherwise empty" "" { target c++ } .-2 } */
  /* { dg-error "forbids flexible array member" "" { target c++ } .-3 } */
};

struct only_fam_2 {
  unsigned int : 2;
  unsigned int : 3;
  int b[];
  /* { dg-error "in a struct with no named members" "" { target c } .-1 } */
  /* { dg-error "in an otherwise empty" "" { target c++ } .-2 } */
  /* { dg-error "forbids flexible array member" "" { target c++ } .-3 } */
};
