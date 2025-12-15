/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-final { scan-tree-dump "loop vectorized" "vect" } } */
/* { dg-additional-options "-O3 -mgeneral-regs-only" { target aarch64*-*-* } } */
/* { dg-additional-options "-O3 -m32 -mno-sse" { target {  { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-additional-options "-w" } */

char sad_memcpy_pout;
int inflate_fast_len;
void inflate_fast() {
  char *out = &sad_memcpy_pout, *from;
  do {
    *out++ = *from++;
    *out++ = *from++;
    inflate_fast_len -= 3;
  } while (inflate_fast_len > 2);
}
