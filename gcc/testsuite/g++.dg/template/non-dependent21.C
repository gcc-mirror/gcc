// PR c++/104507

extern const char *_k_errmsg[];

template<class>
const char* DoFoo(int __r, int __s) {
  const char* n = _k_errmsg[(bool)__r && __s ? 1 : 2];
  return n;
}
