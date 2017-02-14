// { dg-lto-do link }
// { dg-lto-options { "-flto -g" } }
// { dg-extra-ld-options "-r -nostdlib" }

struct a {
  a();
} b;
