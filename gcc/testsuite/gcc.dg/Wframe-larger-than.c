/* Test -Wframe-larger-than for warning
   when the frame size is bigger than specified.
   Origin: Seongbae Park <seongbae.park@gmail.com> */

/* { dg-do compile } */
/* { dg-options "-Wframe-larger-than=2048" } */
/* { dg-skip-if "exceeds eBPF stack limit" { bpf-*-* } } */

extern void func(char *);

void foo (void) {
  char array[4096];
  func(array);
} /* { dg-warning "the frame size of .* bytes is larger than 2048 bytes" } */
