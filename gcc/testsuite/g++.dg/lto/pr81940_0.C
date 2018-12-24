// { dg-lto-do link }
// { dg-require-effective-target lto_incremental }
// { dg-lto-options { { -O -flto } } }
// { dg-extra-ld-options "-r -nostdlib -g" }

int a, b = a;
