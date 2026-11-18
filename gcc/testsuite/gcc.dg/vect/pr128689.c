/* { dg-do compile } */

struct { unsigned char ring[256]; unsigned char after[4096]; } g;

int find(unsigned char i, unsigned char key) {
  int n = 0;
  while (g.ring[i] != key) {
    i++;
    n++;
  }
  return n;
}

/* { dg-final { scan-tree-dump-not "adding no-wrap assumption" "vect" } } */
