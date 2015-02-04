/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-Os -ffixed-rax -ffixed-rbx -ffixed-rcx -ffixed-rdx -ffixed-rdi -ffixed-rsi -ffixed-r8 -ffixed-r9 -ffixed-r10 -ffixed-r11 -ffixed-r12 -ffixed-r13 -ffixed-r14 -ffixed-r15" } */
/* { dg-final { scan-assembler-not "movl\[ \t\]0\\(%.*\\), %.*" } } */

typedef unsigned short uint16_t;
uint16_t a_global;

void __attribute__ ((noinline))
function (uint16_t **a_p)
{
  // unaligned access by address in %rbp: mov    0x0(%rbp),%ebp
  a_global = **a_p;
}

int main(int argc, char **argv)
{
  uint16_t array [4] = { 1, 2, 3, 4 };
  uint16_t *array_elem_p = &array [3];

  function (&array_elem_p);
  return 0;
}
