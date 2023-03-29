/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out)
{
  vint8mf8_t v1 = *(vint8mf8_t*)(in + 1);
  vint16mf4_t v2;
  *(vint8mf8_t*)(out + 1) = v1;
  *(vint16mf4_t*)(out + 2) = v2;
  
  vint8mf8_t v3 = *(vint8mf8_t*)(in + 3);
  vint32mf2_t v4;
  *(vint8mf8_t*)(out + 3) = v3;
  *(vint32mf2_t*)(out + 4) = v4;
  
  vint8mf8_t v5 = *(vint8mf8_t*)(in + 5);
  vint64m1_t v6;
  *(vint8mf8_t*)(out + 5) = v5;
  *(vint64m1_t*)(out + 6) = v6;
  
  vint8mf4_t v7 = *(vint8mf4_t*)(in + 7);
  vint16mf2_t v8;
  *(vint8mf4_t*)(out + 7) = v7;
  *(vint16mf2_t*)(out + 8) = v8;
  
  vint8mf4_t v9 = *(vint8mf4_t*)(in + 9);
  vint32m1_t v10;
  *(vint8mf4_t*)(out + 9) = v9;
  *(vint32m1_t*)(out + 10) = v10;
  
  vint8mf4_t v11 = *(vint8mf4_t*)(in + 11);
  vint64m2_t v12;
  *(vint8mf4_t*)(out + 11) = v11;
  *(vint64m2_t*)(out + 12) = v12;
  
  vint8mf2_t v13 = *(vint8mf2_t*)(in + 13);
  vint16m1_t v14;
  *(vint8mf2_t*)(out + 13) = v13;
  *(vint16m1_t*)(out + 14) = v14;
  
  vint8mf2_t v15 = *(vint8mf2_t*)(in + 15);
  vint32m2_t v16;
  *(vint8mf2_t*)(out + 15) = v15;
  *(vint32m2_t*)(out + 16) = v16;
  
  vint8mf2_t v17 = *(vint8mf2_t*)(in + 17);
  vint64m4_t v18;
  *(vint8mf2_t*)(out + 17) = v17;
  *(vint64m4_t*)(out + 18) = v18;
  
  vint8m1_t v19 = *(vint8m1_t*)(in + 19);
  vint16m2_t v20;
  *(vint8m1_t*)(out + 19) = v19;
  *(vint16m2_t*)(out + 20) = v20;
  
  vint8m1_t v21 = *(vint8m1_t*)(in + 20);
  vint32m4_t v22;
  *(vint8m1_t*)(out + 20) = v21;
  *(vint32m4_t*)(out + 21) = v22;
  
  vint8m1_t v23 = *(vint8m1_t*)(in + 23);
  vint64m8_t v24;
  *(vint8m1_t*)(out + 21) = v23;
  *(vint64m8_t*)(out + 22) = v24;
  
  vint8m2_t v25 = *(vint8m2_t*)(in + 25);
  vint16m4_t v26;
  *(vint8m2_t*)(out + 25) = v25;
  *(vint16m4_t*)(out + 26) = v26;
  
  vint8m2_t v27 = *(vint8m2_t*)(in + 27);
  vint32m8_t v28;
  *(vint8m2_t*)(out + 27) = v27;
  *(vint32m8_t*)(out + 28) = v28;
  
  vint8m4_t v29 = *(vint8m4_t*)(in + 29);
  vint16m8_t v30;
  *(vint8m4_t*)(out + 29) = v29;
  *(vint16m8_t*)(out + 30) = v30;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e64,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e32,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e64,\s*m2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e32,\s*m2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e64,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*m2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e32,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e64,\s*m8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e32,\s*m8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*m8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */

/* { dg-final { scan-assembler-times {vsetvli} 15 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-O2" } } } } */
