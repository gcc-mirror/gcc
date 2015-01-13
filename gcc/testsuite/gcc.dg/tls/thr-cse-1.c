/* { dg-do compile } */
/* { dg-options "-O1" } */
/* Using -mshort-calls avoids loading the function addresses in
   registers and thus getting the counts wrong.  */
/* { dg-additional-options "-mshort-calls" { target epiphany-*-* } } */
/* { dg-require-effective-target tls_emulated } */

/* Test that we only get one call to emutls_get_address when CSE is
   active.  Note that the var _must_ be initialized for the scan asm
   to work, since otherwise there will be an initializer which will,
   correctly, call emutls_get_address.  */
int foo (int b, int c, int d)
{
  static __thread int a=1;
  a += b;
  a -= c;
  a += d;
  return a;
}

/* { dg-final { scan-assembler-not "emutls_get_address.*emutls_get_address.*" { target { ! { "*-wrs-vxworks"  "*-*-darwin8"  "hppa*-*-hpux*" "spu-*-*" "i?86-*-mingw*" "x86_64-*-mingw*" visium-*-* } } } } } */
/* { dg-final { scan-assembler-not "call\tL___emutls_get_address.stub.*call\tL___emutls_get_address.stub.*" { target "*-*-darwin8" } } } */
/* { dg-final { scan-assembler-not "(b,l|bl) __emutls_get_address.*(b,l|bl) __emutls_get_address.*" { target "hppa*-*-hpux*" } } } */
/* { dg-final { scan-assembler-not "(brsl|brasl)\t__emutls_get_address.*(brsl|brasl)\t__emutls_get_address.*" { target spu-*-* } } } */
/* { dg-final { scan-assembler-not "tls_lookup.*tls_lookup.*" { target *-wrs-vxworks } } } */
/* { dg-final { scan-assembler-not "call\t___emutls_get_address.*call\t___emutls_get_address" { target "i?86-*-mingw*" } } } */
/* { dg-final { scan-assembler-not "call\t__emutls_get_address.*call\t__emutls_get_address" { target "x86_64-*-mingw*" } } } */
/* { dg-final { scan-assembler-not "%l __emutls_get_address.*%l __emutls_get_address" { target visium-*-* } } } */
