// PR c++/79592
// { dg-do compile { target c++11 } }

struct pthread_mutex {
  void *m_ptr;
};

struct M {
  pthread_mutex m = { ((void *) 1LL) }; // { dg-error "reinterpret_cast" }
};

constexpr M m;			// { dg-error "M::M" }
