/* CTF generation for bitfields.
   
   In this testcase, two slices are expected - one for enum and the other for
   int.  CTF slices are unnamed records.  */
   
/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x2\[\t \]+\[^\n\]*cts_bits" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x3\[\t \]+\[^\n\]*cts_bits" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*ctt_name" 2 } } */

enum color
{
  RED,
  GREEN,
  BLUE,
  YELLOW,
  ORANGE,
  BLACK
};

struct quickcolor
{
  enum color col:3;
  int brushid:2;
  int strokes;
};

struct quickcolor qc;
