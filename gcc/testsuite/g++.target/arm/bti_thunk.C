/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_pacbti_ok } */
/* { dg-add-options arm_arch_v8_1m_main_pacbti } */
/* { dg-additional-options "-mbranch-protection=bti" }*/

#include <stdio.h>

struct C18 {
  virtual void f7();
};

struct C19 : virtual C18 {
  virtual void f7();
};

void C19::f7() {
  printf("foo\n");
}

/* { dg-final { scan-assembler-times "\tbti" 2 } } */
