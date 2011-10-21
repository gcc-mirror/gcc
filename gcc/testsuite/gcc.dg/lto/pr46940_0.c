/* { dg-require-linker-plugin "" } */
/* { dg-extra-ld-options "-fuse-linker-plugin" } */
#include <stdio.h>

#define ASMNAME(cname)  ASMNAME2 (__USER_LABEL_PREFIX__, cname)
#define ASMNAME2(prefix, cname) STRING (prefix) cname
#define STRING(x)    #x

extern __attribute__((visibility("hidden"))) void _moz_foo (void);
extern __typeof (_moz_foo) _moz_foo __asm__ (ASMNAME ("INT__foo")) __attribute__((__visibility__("hidden"))) ;
void _moz_foo(void)
{
  printf ("blah\n");
}
extern __typeof (_moz_foo) EXT__foo __asm__(ASMNAME ("_moz_foo")) __attribute__((__alias__("" "INT__foo")));
