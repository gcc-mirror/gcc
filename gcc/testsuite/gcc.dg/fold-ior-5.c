/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-require-effective-target int32plus } */

unsigned int test_ior(unsigned char i)
{
  return (i | (i<<16)) | ((i<<24) | (i<<8));
}

unsigned int test_xor(unsigned char i)
{
  return (i ^ (i<<16)) ^ ((i<<24) ^ (i<<8));
}

/* { dg-final { scan-tree-dump-not " \\^ " "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\| " "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\* 16843009" 2 "optimized" } } */

