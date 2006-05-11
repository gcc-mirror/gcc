/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-O2 -maltivec -mabi=altivec" } */
/* { dg-final { scan-assembler-not "lvx" } } */
#include <altivec.h>

void foo (vector int);
void foo_s (vector short);
void foo_c (vector char);

/* All constants should be loaded into vector register without
   load from memory.  */
void
bar (void) 
{
  foo ((vector int) {0, 0, 0, 0});
  foo ((vector int) {1, 1, 1, 1});
  foo ((vector int) {15, 15, 15, 15});
  foo ((vector int) {-16, -16, -16, -16});
  foo ((vector int) {0x10001, 0x10001, 0x10001, 0x10001});
  foo ((vector int) {0xf000f, 0xf000f, 0xf000f, 0xf000f});
  foo ((vector int) {0xfff0fff0, 0xfff0fff0, 0xfff0fff0, 0xfff0fff0});
  foo ((vector int) {0x1010101, 0x1010101, 0x1010101, 0x1010101});  
  foo ((vector int) {0xf0f0f0f, 0xf0f0f0f, 0xf0f0f0f, 0xf0f0f0f});  
  foo ((vector int) {0xf0f0f0f0, 0xf0f0f0f0, 0xf0f0f0f0, 0xf0f0f0f0});
  foo ((vector int) {0x10101010, 0x10101010, 0x10101010, 0x10101010});
  foo ((vector int) {0x1e1e1e1e, 0x1e1e1e1e, 0x1e1e1e1e, 0x1e1e1e1e});
  foo ((vector int) {0x100010, 0x100010, 0x100010, 0x100010});
  foo ((vector int) {0x1e001e, 0x1e001e, 0x1e001e, 0x1e001e});
  foo ((vector int) {0x10, 0x10, 0x10, 0x10});
  foo ((vector int) {0x1e, 0x1e, 0x1e, 0x1e});

  foo_s ((vector short int) {0, 0, 0, 0, 0, 0, 0, 0});
  foo_s ((vector short int) {1, 1, 1, 1, 1, 1, 1, 1});
  foo_s ((vector short int) {15, 15, 15, 15, 15, 15, 15, 15});
  foo_s ((vector short int) {-16, -16, -16, -16, -16, -16, -16, -16});
  foo_s ((vector short int) {0xf0f0, 0xf0f0, 0xf0f0, 0xf0f0, 
			       0xf0f0, 0xf0f0, 0xf0f0, 0xf0f0});
  foo_s ((vector short int) {0xf0f, 0xf0f, 0xf0f, 0xf0f, 
			       0xf0f, 0xf0f, 0xf0f, 0xf0f});
  foo_s ((vector short int) {0x1010, 0x1010, 0x1010, 0x1010, 
			       0x1010, 0x1010, 0x1010, 0x1010});
  foo_s ((vector short int) {0x1e1e, 0x1e1e, 0x1e1e, 0x1e1e, 
			       0x1e1e, 0x1e1e, 0x1e1e, 0x1e1e});

  foo_c ((vector char) {0, 0, 0, 0, 0, 0, 0, 0,
			  0, 0, 0, 0, 0, 0, 0, 0});
  foo_c ((vector char) {1, 1, 1, 1, 1, 1, 1, 1, 
			  1, 1, 1, 1, 1, 1, 1, 1});
  foo_c ((vector char) {15, 15, 15, 15, 15, 15, 15, 15,
			  15, 15, 15, 15, 15, 15, 15, 15});
  foo_c ((vector char) {-16, -16, -16, -16, -16, -16, -16, -16,
			  -16, -16, -16, -16, -16, -16, -16, -16});
  foo_c ((vector char) {16, 16, 16, 16, 16, 16, 16, 16,
			  16, 16, 16, 16, 16, 16, 16, 16});
  foo_c ((vector char) {30, 30, 30, 30, 30, 30, 30, 30,
			  30, 30, 30, 30, 30, 30, 30, 30});

}
