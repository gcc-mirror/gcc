/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+simd+fp" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler-times {\.arch armv8\-a} 1 } } */

 /* Check that grouping of bits that don't form a synthetic group don't turn
    on the parent. e.g. rdma turns on simd+fp, but simd+fp does not turn on
    rdma since rdma is it's own group.  crypto however turns on aes and sha2
    and turning on sha2 and eas should turn on crypto!.  */
