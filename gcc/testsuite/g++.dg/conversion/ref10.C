// PR c++/114561

void create(void* u) {
  const void* const& r = ( (void)0, u );
}
