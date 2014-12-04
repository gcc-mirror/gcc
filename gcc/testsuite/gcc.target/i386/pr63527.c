/* PR rtl-optimization/pr63527 */
/* { dg-do compile { target { ia32 && fpic } } } */
/* { dg-options "-O2 -fPIC" } */

struct cache_file
{
  char magic[sizeof "ld.so-1.7.0" - 1];
  unsigned int nlibs;
};
typedef unsigned int size_t;
size_t cachesize __attribute__ ((visibility ("hidden")));
struct cache_file *cache __attribute__ ((visibility ("hidden")));
extern int __munmap (void *__addr, size_t __len);
void
_dl_unload_cache (void)
{
  if (cache != ((void *)0) && cache != (struct cache_file *) -1)
    {
      __munmap (cache, cachesize);
      cache = ((void *)0) ;
    }
}

/* We shouldn't load EBX again.  */
/* { dg-final { scan-assembler-not "movl\[ \t\]%\[^,\]+, %ebx" } } */
