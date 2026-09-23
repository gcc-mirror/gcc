/* { dg-do link }  */
/* { dg-require-effective-target arm_v8_1m_mve_link } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2 -flto" } */

#include <arm_mve.h>

int main(int argc, char* argv[])
{
  return vaddvq(__arm_vdupq_n_s8 (argc));
}
