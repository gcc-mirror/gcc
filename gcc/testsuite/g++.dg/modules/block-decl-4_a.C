// PR c++/123627
// { dg-additional-options "-fmodules -fdump-lang-module-alias" }
// { dg-module-cmi m:part }

export module m:part;

auto foo() {
  struct S {} s;
  return s;
}

inline auto inline_foo() {
  struct S {} s;
  return s;
}

// { dg-final { scan-lang-dump {Wrote:-[0-9]*'s attached merge key type_decl:'::foo::S'} module } }
// { dg-final { scan-lang-dump {Wrote:-[0-9]*'s local type merge key type_decl:'::inline_foo::S'} module } }

auto bar() {
  auto lambda = []{};
  return lambda;
}

inline auto inline_bar() {
  auto lambda = []{};
  return lambda;
}

// { dg-final { scan-lang-dump {Wrote:-[0-9]*'s attached merge key type_decl:'::bar::._anon_[0-9]*'} module } }
// { dg-final { scan-lang-dump {Wrote:-[0-9]*'s local type merge key type_decl:'::inline_bar::._anon_[0-9]*'} module } }

auto qux() {
  struct XN {
    auto inner() {
      struct YNN {} y;
      return y;
    }
    inline auto inline_inner() {
      struct YNI {} y;
      return y;
    }
  } x;
  return x;
}

inline auto inline_qux() {
  struct XI {
    auto inner() {
      struct YIN {} y;
      return y;
    }
    inline auto inline_inner() {
      struct YII {} y;
      return y;
    }
  } x;
  return x;
}

// If the innermost function has no definition ('N'), merge via atached key decls;
// even if an outer function does have an inline body, we can't see this definition.
// { dg-final { scan-lang-dump {Wrote:-[0-9]*'s attached merge key type_decl:'::qux::XN'} module } }
// { dg-final { scan-lang-dump {Wrote:-[0-9]*'s attached merge key type_decl:'::qux::XN::inner::YNN'} module } }
// { dg-final { scan-lang-dump {Wrote:-[0-9]*'s attached merge key type_decl:'::inline_qux::XI::inner::YIN'} module } }

// But if the innermost function has an emitted definition then we can use it.
// { dg-final { scan-lang-dump {Wrote:-[0-9]*'s local type merge key type_decl:'::qux::XN::inline_inner::YNI'} module } }
// { dg-final { scan-lang-dump {Wrote:-[0-9]*'s local type merge key type_decl:'::inline_qux::XI'} module } }
// { dg-final { scan-lang-dump {Wrote:-[0-9]*'s local type merge key type_decl:'::inline_qux::XI::inline_inner::YII'} module } }
