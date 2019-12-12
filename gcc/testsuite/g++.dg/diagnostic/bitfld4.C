// PR c++/92215 - flawed diagnostic for bit-field with non-integral type.
// { dg-do compile { target c++11 } }

struct S {
  int *f1 : 3; // { dg-error "bit-field .f1. has non-integral type .int\\*." }
  int &f2 : 3; // { dg-error "bit-field .f2. has non-integral type .int&." }
  int &&f3 : 3; // { dg-error "bit-field .f3. has non-integral type .int&&." }
  int f4[1] : 3; // { dg-error "bit-field .f4. has non-integral type .int \\\[1\\\]." }
  int *f5 __attribute__((deprecated)) : 3; // { dg-error "bit-field .f5. has non-integral type .int\\*." }
  int f6[1] __attribute__((deprecated)) : 3; // { dg-error "bit-field .f6. has non-integral type .int \\\[1\\\]." }
  int &f7 __attribute__((deprecated)): 3; // { dg-error "bit-field .f7. has non-integral type .int&." }
  int ****: 3; // { dg-error "expected" }
  int *f9[1] : 3; // { dg-error "bit-field .f9. has non-integral type .int\\* \\\[1\\\]." }
  int (*f10)() : 3; // { dg-error "bit-field .f10. has non-integral type .int \\(\\*\\)\\(\\)." }
  int [][2] : 3; // { dg-error "expected" }
};
