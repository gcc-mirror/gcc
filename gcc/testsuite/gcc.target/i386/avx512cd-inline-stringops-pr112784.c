/* { dg-do compile } */
/* { dg-options "-mavx512cd -finline-stringops" } */

struct S {
  int e;
} __attribute__((aligned(128)));

int main() {
  struct S s1;
  struct S s2;
  int v = __builtin_memcmp(&s1, &s2, sizeof(s1));
}
