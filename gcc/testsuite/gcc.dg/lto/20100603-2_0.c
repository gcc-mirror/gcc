/* { dg-lto-do link } */
/* { dg-extra-ld-options {-r -nostdlib -flinker-output=nolto-rel} } */

int foo;
int *i = &foo;
