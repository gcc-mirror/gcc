/* { dg-require-linker-plugin "" } */
/* { dg-extra-ld-options "-fuse-linker-plugin" } */
#include <stdio.h>

extern __attribute__((visibility("hidden"))) void _moz_foo (void);
extern __typeof (_moz_foo) _moz_foo __asm__ ("" "INT__foo") __attribute__((__visibility__("hidden"))) ;
void _moz_foo(void)
{
  printf ("blah\n");
}
extern __typeof (_moz_foo) EXT__foo __asm__("" "_moz_foo") __attribute__((__alias__("" "INT__foo")));
