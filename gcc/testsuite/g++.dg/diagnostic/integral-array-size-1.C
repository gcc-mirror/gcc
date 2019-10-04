template<typename T>
void foo(T a)
{
  new int[a];  // { dg-error "11:size in array new must have integral type" }
}

template void foo(float);
