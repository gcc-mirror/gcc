/* PR middle-end/81737 */
/* { dg-do compile } */
/* { dg-options "" } */

extern int a[];
void fn1() { (a + 0)[1]; }
