// PR c++/29735
// { dg-do compile }

int __attribute__ ((vector_size (8))) main () // { dg-error "must return" }
{
  return 0;
}
