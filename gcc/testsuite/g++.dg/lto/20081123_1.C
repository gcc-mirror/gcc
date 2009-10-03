struct foo {
  const int* int_array;
  const void* default_instance;
};
struct bar {
  static const bar& _default_instance;
  static const foo _internal_foo;
};
struct quxx {
  static int trouble[];
};
int quxx::trouble[] = { };
const foo bar::_internal_foo = { 
  quxx::trouble,
  &bar::_default_instance
};
