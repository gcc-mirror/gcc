/* PR tree-optimization/96915 */
/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve" } */

#pragma GCC aarch64 "arm_sve.h"
void b() {
  switch (svcntd())
  case 2:
  case 4:
    b();
}
