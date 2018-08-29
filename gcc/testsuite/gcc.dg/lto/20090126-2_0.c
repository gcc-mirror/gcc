/* { dg-lto-do link } */
/* { dg-require-effective-target fpic } */
/* { dg-lto-options {{-fPIC -O2 -flto -flto-partition=1to1}} } */
/* { dg-extra-ld-options {-fno-PIC -r -nostdlib -O2 -flto -flto-partition=1to1 -flinker-output=nolto-rel} } */

int main(int argc, char **argv) {
  return 0;
}
