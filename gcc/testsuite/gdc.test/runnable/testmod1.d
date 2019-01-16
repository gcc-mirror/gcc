// COMPILE_SEPARATELY
// EXTRA_SOURCES: imports/testmod1a.d imports/testmod1b.d
// PERMUTE_ARGS:

struct Foo(T) {
  void foo(T arg) { }
}


int main()
{
    return 0;
}
