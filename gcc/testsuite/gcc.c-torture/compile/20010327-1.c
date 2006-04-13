/* { dg-skip-if "non-SI pointers" { m32c-*-* } { "*" } { "" } } */

/* This testcase tests whether GCC can produce static initialized data
   that references addresses of size 'unsigned long', even if that's not
   the same as __SIZE_TYPE__.  (See 20011114-1.c for the same test of
   size __SIZE_TYPE__.)  

   Some rare environments might not have the required relocs to support
   this; they should have this test disabled in the .x file.  */

extern void _text;
static unsigned long x = (unsigned long) &_text - 0x10000000L - 1;
