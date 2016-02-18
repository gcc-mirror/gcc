/* PR middle-end/69801 */
/* { dg-do compile } */
/* { dg-options "" } */

struct {
    char c[1];
} b, c;
int d, e;
void fn1() { e ? (d ? b : c).c : (d ? b : c).c; }
