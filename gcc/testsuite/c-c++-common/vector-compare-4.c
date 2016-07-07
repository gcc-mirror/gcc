/* PR c/68062 */
/* { dg-do compile } */
/* { dg-options "-Wsign-compare -Wno-psabi" } */
/* Ignore warning on some powerpc configurations. */
/* { dg-prune-output "non-standard ABI extension" } */

typedef signed char __attribute__ ((vector_size (4))) v4qi;
typedef unsigned char __attribute__ ((vector_size (4))) uv4qi;
typedef signed int __attribute__ ((vector_size (4 * __SIZEOF_INT__))) v4si;
typedef unsigned int __attribute__ ((vector_size (4 * __SIZEOF_INT__))) uv4si;

v4qi
fn1 (void)
{
  v4qi a = { 1, 2, 3, 4 };
  uv4qi b = { 4, 3, 2, 1 };
  v4qi v = { 0, 0, 0, 0 };

  v += (a == b); /* { dg-warning "comparison between types" } */
  v += (a != b); /* { dg-warning "comparison between types" } */
  v += (a >= b); /* { dg-warning "comparison between types" } */
  v += (a <= b); /* { dg-warning "comparison between types" } */
  v += (a > b); /* { dg-warning "comparison between types" } */
  v += (a < b); /* { dg-warning "comparison between types" } */

  return v;
}

v4si
fn2 (void)
{
  v4si a = { 1, 2, 3, 4 };
  uv4si b = { 4, 3, 2, 1 };
  v4si v = { 0, 0, 0, 0 };

  v += (a == b); /* { dg-warning "comparison between types" } */
  v += (a != b); /* { dg-warning "comparison between types" } */
  v += (a >= b); /* { dg-warning "comparison between types" } */
  v += (a <= b); /* { dg-warning "comparison between types" } */
  v += (a > b); /* { dg-warning "comparison between types" } */
  v += (a < b); /* { dg-warning "comparison between types" } */

  return v;
}
