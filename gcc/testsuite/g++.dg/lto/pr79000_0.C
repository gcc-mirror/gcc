// { dg-lto-do link }
// { dg-require-effective-target lto_incremental }
// { dg-lto-options { "-flto -g" } }
// { dg-extra-ld-options "-r -nostdlib" }

struct a {
  a();
} b;
