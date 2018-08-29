/* { dg-lto-do link } */
/* { dg-lto-options {{-flto -flto-partition=1to1 -r -nostdlib}} } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */
void bar(void) {}
