#ifndef HAVE_DEFINED_PR120652_H
#define HAVE_DEFINED_PR120652_H

unsigned n;
char ab[6];
unsigned ac;
unsigned ae;

int ak(int bb) {
bd:
  for (ac = -17; ac != 16; ac++) {
    unsigned be = 95;
    if (be <= n) {
      char *bg = &ab[1];
      *bg ^= bb;
    } else {
      ae--;
      for (n = 8; 0;)
	goto bd;
    }
  }
  return 0;
}

int main() {
  ak(7);

  return 0;
}

#endif
