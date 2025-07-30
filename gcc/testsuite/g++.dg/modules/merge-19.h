// PR c++/121238

inline void inc(const char*& __first) {
  ++__first;
}

template <typename = void>
bool parse_integer(const char *first) {
  const char *start = first;
  inc(first);
  return first != start;
}
template bool parse_integer<void>(const char*);


struct S { ~S() {} int x; };
template <typename = void>
bool take_by_invisiref(S s) {
  return s.x == 5;
}
template bool take_by_invisiref<void>(S);
