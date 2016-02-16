/* { dg-do compile } */

struct {
    char c[1];
} b, c;
int d, e;
void fn1() { e ? (d ? b : c).c : (d ? b : c).c; }
