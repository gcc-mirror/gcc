/* { dg-lto-do link } */
/* { dg-lto-options {{-fPIC -O2 -fwhopr}} } */
/* { dg-extra-ld-options {-fno-PIC -r -nostdlib -O2 -fwhopr} } */

int main(int argc, char **argv) {
  return 0;
}
