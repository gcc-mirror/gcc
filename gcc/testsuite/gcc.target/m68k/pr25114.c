/* { dg-do compile } */
/* { dg-options "-O2 -m68040" } */
/* There should be 16 logical right shift instructions.  One for each function*/
/* { dg-final { scan-assembler-times "lsr" 16 } } */

unsigned int bar (void);

#define F(C) void foo##C (void) { unsigned int a = bar (); if (a <= C) bar (); }
#define G(C) void foo2##C (void) { unsigned int a = bar (); if (a > C) bar (); }

F(0x1)
F(0x3)
F(0x7)
F(0xf)
F(0x1f)
F(0x3f)
F(0x7f)
F(0xff)
G(0x1)
G(0x3)
G(0x7)
G(0xf)
G(0x1f)
G(0x3f)
G(0x7f)
G(0xff)
