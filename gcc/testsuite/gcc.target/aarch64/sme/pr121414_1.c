#pragma GCC target "+sme2"

void f1() __arm_streaming_compatible {}
void f2() __arm_streaming {}
void f3() __arm_in("za") {}
void f4() __arm_out("za") {}
void f5() __arm_inout("za") {}
void f6() __arm_in("zt0") {}
void f7() __arm_out("zt0") {}
void f8() __arm_inout("zt0") {}

__arm_locally_streaming void g1() {}
__arm_new("za") void g2() {}
__arm_new("zt0") void g3() {}

/* { dg-final { scan-assembler {\t\.variant_pcs\tf1\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tf2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tf3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tf4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tf5\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tf6\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tf7\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tf8\n} } } */

/* { dg-final { scan-assembler-not {\t\.variant_pcs\tg1\n} } } */
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tg2\n} } } */
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tg3\n} } } */
