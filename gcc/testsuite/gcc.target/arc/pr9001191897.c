/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=archs -Os -fpic -mno-sdata -mno-indexed-loads -w" } */
a;
c() {
  static char b[25];
  for (; a >= 0; a--)
    if (b[a])
      b[a] = '\0';
}
