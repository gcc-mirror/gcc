// { dg-options "-w" }

union u1 {
  char m1 : 16;
} x;

int main () {
  x.m1 = 256;
}
