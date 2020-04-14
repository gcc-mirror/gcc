/* PR c/92326 - wrong bound in zero-length array diagnostics
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

extern int a0[0];
extern int ax[];

void warn_global_array (void)
{
  a0[0] = 0;        // { dg-warning "array bounds of 'int *\\\[0]'" }
  ax[-1] = 0;       // { dg-warning "array bounds of 'int *\\\[]'" }
}


struct S0 { int n, a0[0]; } s0;
struct Sx { int n, ax[]; } sx = { 0 };

void warn_member_array (void)
{
  s0.a0[0] = 0;     // { dg-warning "array bounds of 'int *\\\[0]'" }
  sx.ax[0] = 0;     // { dg-warning "array bounds of 'int *\\\[]'" }
}
