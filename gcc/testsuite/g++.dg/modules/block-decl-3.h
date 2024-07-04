// GMF

// Non-inline function definitions in headers are a recipe for ODR violations,
// but we should probably support that anyway as its not inherently wrong
// if only ever included into the GMF of a single module.

auto gmf_n_i() {
  struct X { void f() {} };
  return X{};
}

inline auto gmf_i_i() {
  struct X { void f() {} };
  return X{};
}

auto gmf_n_i_i() {
  struct X {
    auto f() {
      struct Y {
	void g() {}
      };
      return Y{};
    }
  };
  return X{};
}

inline auto gmf_i_i_i() {
  struct X {
    auto f() {
      struct Y {
	void g() {}
      };
      return Y{};
    }
  };
  return X{};
}
