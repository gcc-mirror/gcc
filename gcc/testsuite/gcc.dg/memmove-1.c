/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "memmove" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
static const char a[100]={1,2,3,4};
char b[1000];
int i,i1;
static inline void
__attribute__ ((always_inline))
domem (void *dest, const void *src, int len)
{
	__builtin_memmove (dest, src, len);
}
t()
{
	domem (b,a,100);
	domem (b+i1,(const void *)b,1);
}
