// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi mod }

// Test that we can link various forms of local class functions.
// Function names use i=inline, n=non-inline, for each nesting.

module;
#include "block-decl-3.h"

export module mod;

namespace {
  void internal() {}
}

// singly-nested

export auto n_n() {
  internal();
  struct X { void f() { internal(); } };
  return X{};
}

export auto n_i() {
  internal();
  struct X { inline void f() {} };
  return X{};
}

export inline auto i_n() {
  // `f` is not inline here, so this is OK
  struct X { void f() { internal(); } };
  return X{};
}

export inline auto i_i() {
  struct X { inline void f() {} };
  return X{};
}


// doubly nested

export auto n_n_n() {
  struct X {
    auto f() {
      struct Y {
	void g() { internal(); }
      };
      return Y{};
    }
  };
  return X{};
}

export auto n_i_n() {
  struct X {
    inline auto f() {
      struct Y {
	void g() { internal(); }
      };
      return Y{};
    }
  };
  return X{};
}

export inline auto i_n_i() {
  struct X {
    auto f() {
      struct Y {
	inline void g() {}
      };
      return Y {};
    }
  };
  return X{};
}

export inline auto i_i_i() {
  struct X {
    inline auto f() {
      struct Y {
	inline void g() {}
      };
      return Y{};
    }
  };
  return X{};
}


// multiple types

export auto multi_n_n() {
  struct X {
    void f() { internal(); }
  };
  struct Y {
    X x;
  };
  return Y {};
}

export auto multi_n_i() {
  struct X {
    inline void f() {}
  };
  struct Y {
    X x;
  };
  return Y {};
}

export inline auto multi_i_i() {
  struct X {
    inline void f() {}
  };
  struct Y {
    X x;
  };
  return Y {};
};


// extern "C++"

export extern "C++" auto extern_n_i() {
  struct X {
    void f() {}  // implicitly inline
  };
  return X{};
};

export extern "C++" inline auto extern_i_i() {
  struct X {
    void f() {}
  };
  return X{};
};


// GMF

export using ::gmf_n_i;
export using ::gmf_i_i;
export using ::gmf_n_i_i;
export using ::gmf_i_i_i;


// can access from implementation unit

auto only_used_in_impl() {
  struct X { void f() {} };
  return X{};
}
export void test_from_impl_unit();
