/* { dg-do compile } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */

struct s1 {
  int *p;
  int i;
};

struct s2 {
  struct s1 s;
  int i;
};

int test (struct s2 s, ...) { }
