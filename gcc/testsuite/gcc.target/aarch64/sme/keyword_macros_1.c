/* { dg-options "-std=c90 -pedantic-errors" } */

void f1 () __arm_streaming;
void f2 () __arm_streaming_compatible;
void f3 () __arm_in("za");
void f4 () __arm_out("za");
void f5 () __arm_inout("za");
void f6 () __arm_preserves("za");
__arm_new("za") void f7 () {}
__arm_locally_streaming void f8 () {}
