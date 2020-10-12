/* { dg-do compile } */
/* { dg-options "-O1" } */

struct a {
  int b;
};
int main() {
  struct a d[][6] = {4};
  struct a e;
  d[1955249013][1955249013] = e;
  return e.b;
}
