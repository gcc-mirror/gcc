/* { dg-lto-do link } */
/* { dg-lto-options {{-O0 -flto -flto-partition=1to1}} } */
/* { dg-extra-ld-options {-r -nostdlib -O2 -flto -flto-partition=1to1} } */

int main(int argc, char **argv) {
  return 0;
}
