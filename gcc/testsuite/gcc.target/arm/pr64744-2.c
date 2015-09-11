/* { dg-do compile } */
/* { dg-require-effective-target naked_functions } */
/* { dg-options "-O0" } */

struct s {
  char a;
    int b;
};

__attribute__((naked))
void foo () {
  struct s x = {}; /* { dg-error "cannot allocate stack for variable" } */
}
