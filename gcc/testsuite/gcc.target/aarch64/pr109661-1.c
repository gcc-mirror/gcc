/* { dg-options "-O2 -Wpsabi" } */

enum __attribute__((aligned(16))) e { E };

enum e test (int x, enum e y) { return y; }
