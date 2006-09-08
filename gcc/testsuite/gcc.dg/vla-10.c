/* ICE with VLA in nested parameter declaration: should be treated
   like [*] instead of the size being expanded.  Bug 28504 from Volker
   Reichelt <reichelt@gcc.gnu.org>.  */
/* { dg-do compile } */
/* { dg-options "" } */

void foo(void (*p)(int n, int x[n])) {}
