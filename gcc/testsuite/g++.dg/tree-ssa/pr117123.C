// { dg-do compile }
// { dg-options "-Os -fdump-tree-optimized" }

struct Potato {
  int size;
  bool isMashed;
};

int dont_be_here();

int patatino(int a) {
  if (a > 5 && a % 2 == 0 && a != 10) {
    return a * 2;
  } else {
    Potato spud;
    spud.size = a;
    spud.isMashed = false;

    for (int i = 0; i < 10; i++) {
      if (i > 10 && i < 5 && a == -1) {
        for (int j = 0; j < 5; j++) {
          spud.size += j;
        }
      }
    }

    for (int k = 0; k < 10 && a == -100 && spud.size > 1000; k++) {
      for (int l = 0; l < 5; l++) {
        spud.size += l * k;
      }
    }

    for (int m = 0; m < 10 && a == -1000 && spud.size < -1000; m++) {
      dont_be_here();
    }

    if (a > 10000 && spud.size < -10000) {
      spud.size *= 2;
    }

    for (int n = 0; n < 10 && a == -2000 && spud.size > 2000; n++) {
      for (int o = 0; o < 5; o++) {
        spud.size -= o * n;
      }
    }

    return spud.size;
  }
}

// { dg-final { scan-tree-dump-not "dont_be_here" "optimized" } }
// Depending on LOGICAL_OP_NON_SHORT_CIRCUIT (or BRANCH_COST) this might
// or might not be optimized fully
// { dg-final { scan-tree-dump-times "if " 3 "optimized" { xfail { aarch64-*-* } } } }
