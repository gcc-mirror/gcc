// PR c++/30022
// { dg-do compile }

void foo()
{
  int __attribute__((vector_size(8))) v;
  v = 1/v;
}
