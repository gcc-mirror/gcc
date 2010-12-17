/* Test negative forms of various options are rejected.  */
/* { dg-do compile } */
/* { dg-options "-Wno-strict-aliasing=1 -Wno-strict-overflow=1 -fno-abi-version=1 -fno-lto-compression-level=1 -fno-tree-parallelize-loops=1" } */

/* { dg-error "-fno-abi-version" "-fno-abi-version" { target *-*-* } 0 } */
/* { dg-error "-fno-lto-compression-level" "-fno-lto-compression-level" { target *-*-* } 0 } */
/* { dg-error "-fno-tree-parallelize-loops" "-fno-tree-parallelize-loops" { target *-*-* } 0 } */
/* { dg-error "-Wno-strict-overflow" "-Wno-strict-overflow" { target *-*-* } 0 } */
/* { dg-error "-Wno-strict-aliasing" "-Wno-strict-aliasing" { target *-*-* } 0 } */
