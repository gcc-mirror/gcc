/* PR middle-end/90899 */
/* { dg-do compile } */
/* { dg-require-ifunc "" } */

__attribute__ ((target_clones ("default", "arch=slm"))) static int f () { return 0; }
__attribute__ ((alias ("f"))) __typeof (f) g;
