volatile int i __attribute__((vector_size(8)));

void foo()
{
  i += i; // { dg-warning "deprecated" "" { target c++2a } }
}
