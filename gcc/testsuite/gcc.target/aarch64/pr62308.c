/* { dg-do compile } */
/* { dg-options "-mbig-endian" } */

typedef int __attribute__((vector_size(16))) v4si;
struct S2823 {v4si a;int b[0];};
void checkx2823 (struct S2823 args){};
