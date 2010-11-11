// { dg-lto-do link }
// { dg-lto-options {{-flto -flto-partition=1to1 -fPIC}} }
// { dg-extra-ld-options "-flto -flto-partition=1to1 -r -nostdlib" }

int X;
