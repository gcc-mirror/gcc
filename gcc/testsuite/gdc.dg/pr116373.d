// { dg-do compile }
int[] x;

void foo (int[] y = x[]) {}

void main () {
    foo();
}
