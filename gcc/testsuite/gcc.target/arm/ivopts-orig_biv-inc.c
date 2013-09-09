/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */
/* { dg-skip-if "" { arm_thumb1 } } */

extern char *__ctype_ptr__;

unsigned char * foo(unsigned char *ReadPtr)
{

 unsigned char c;

 while (!(((__ctype_ptr__+sizeof(""[*ReadPtr]))[(int)(*ReadPtr)])&04) == (!(0)))
  ReadPtr++;

 return ReadPtr;
}

/* { dg-final { scan-tree-dump-times "original biv" 2 "ivopts"} } */
/* { dg-final { cleanup-tree-dump "ivopts" } } */
