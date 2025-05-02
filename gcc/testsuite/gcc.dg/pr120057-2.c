/* PR c/120057 */
/* { dg-do compile } */
/* { dg-options "-std=c23" } */

constexpr signed char foo[65] = {
  1, 2, 3, 4, 5, 6, 7, 8, 9, 129, 11, 12, 13, 14, 15, 16,	/* { dg-error "'constexpr' initializer not representable in type of object" } */
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
  1
};
