/* PR tree-optimization/97736 */
/* { dg-do compile { target { { x86_64-*-* aarch64-*-* ia64-*-* powerpc64-*-* } && lp64 } } } */
/* { dg-options "-O2 -fdump-tree-switchlower1" } */

bool is_vowel(char c) {
    switch (c)
  case'a':case'e':case'i':case'o':case'u':
      return true;
      return false;
}

/* { dg-final { scan-tree-dump ";; GIMPLE switch case clusters: BT:97-117" "switchlower1" } } */
