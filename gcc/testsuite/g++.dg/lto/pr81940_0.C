// { dg-lto-do link }
// { dg-lto-options { { -O -flto } } }
// { dg-extra-ld-options "-r -nostdlib -g" }

int a, b = a;
