template<typename> void foo()
{
  int i __attribute__((vector_size(2*sizeof(int))));
  (void) __builtin_shuffle(i, i);
}
