// { dg-do compile { target c++11 } }
// Minimized from the testcase for PR c++/88146,
// then turned into multiple variants. 

// We issue an error when calling an inherited ctor with at least one
// argument passed through a varargs ellipsis, if the call is in an
// evaluated context.  Even in nonevaluated contexts, we will
// instantiate constexpr templates (unlike non-constexpr templates),
// which might then issue errors that in nonevlauated contexts
// wouldn't be issued.

// In these variants, the inherited ctor is constexpr, but it's only
// called in unevaluated contexts, so no error is issued.  The
// templateness of the ctor doesn't matter, because the only call that
// passes args through the ellipsis is in a noexcept expr, that is not
// evaluated.  The ctors in derived classes are created and
// instantiated, discarding arguments passed through the ellipsis when
// calling base ctors, but that's not reported: we only report a
// problem when *calling* ctors that behave this way.
namespace unevaled_call {
  namespace no_arg_before_ellipsis {
    namespace without_template {
      struct foo {
	constexpr foo(...) {}
      };
      struct boo : foo {
	using foo::foo;
      };
      struct bar : boo {
	using boo::boo;
      };
      void f() noexcept(noexcept(bar{0}));
    }

    namespace with_template {
      struct foo {
	template <typename... T>
	constexpr foo(...) {}
      };
      struct boo : foo {
	using foo::foo;
      };
      struct bar : boo {
	using boo::boo;
      };
      void f() noexcept(noexcept(bar{0}));
    }
  }

  namespace one_arg_before_ellipsis {
    namespace without_template {
      struct foo {
	constexpr foo(int, ...) {}
      };
      struct boo : foo {
	using foo::foo;
      };
      struct bar : boo {
	using boo::boo;
      };
      void f() noexcept(noexcept(bar{0,1}));
    }

    namespace with_template {
      struct foo {
	template <typename T>
	constexpr foo(T, ...) {}
      };
      struct boo : foo {
	using foo::foo;
      };
      struct bar : boo {
	using boo::boo;
      };
      void f() noexcept(noexcept(bar{0,1}));
    }
  }
}

// In these variants, the inherited ctor is constexpr, and it's called
// in unevaluated contexts in ways that would otherwise trigger the
// sorry message.  Here we check that the message is not issued at
// those calls, nor at subsequent calls that use the same ctor without
// passing arguments through its ellipsis.  We check that it is issued
// later, when we pass the ctor arguments through the ellipsis.
namespace evaled_bad_call_in_u {
  namespace one_arg_before_ellipsis {
    namespace without_template {
      struct foo {
	constexpr foo(int, ...) {}
      };
      struct boo : foo {
	using foo::foo;
      };
      struct bar : boo {
	using boo::boo;
      };
      void f() noexcept(noexcept(bar{0, 1}));
      bar t(0);
      bar u(0, 1); // { dg-message "sorry, unimplemented: passing arguments to ellipsis" }
    }

    namespace with_template {
      struct foo {
	template <typename T>
	constexpr foo(T, ...) {}
      };
      struct boo : foo {
	using foo::foo;
      };
      struct bar : boo {
	using boo::boo;
      };
      void f() noexcept(noexcept(bar{0,1}));
      bar t(0);
      bar u(0,1); // { dg-message "sorry, unimplemented: passing arguments to ellipsis" }
    }
  }

  namespace no_arg_before_ellipsis {
    namespace without_template {
      struct foo {
	constexpr foo(...) {}
      };
      struct boo : foo {
	using foo::foo;
      };
      struct bar : boo {
	using boo::boo;
      };
      void f() noexcept(noexcept(bar{0}));
      bar u(0); // { dg-message "sorry, unimplemented: passing arguments to ellipsis" }
    }

    namespace with_template {
      struct foo {
	template <typename... T>
	constexpr foo(...) {}
      };
      struct boo : foo {
	using foo::foo;
      };
      struct bar : boo {
	using boo::boo;
      };
      void f() noexcept(noexcept(bar{0}));
      bar u(0); // { dg-message "sorry, unimplemented: passing arguments to ellipsis" }
    }
  }
}

// Now, instead of instantiating a class that uses a derived ctor, we
// introduce another template ctor that will use the varargs ctor to
// initialize its base class.  The idea is to verify that the error
// message is issued, even if the instantiation occurs in a
// nonevaluated context, e.g., for constexpr templates.  In the
// inherited_derived_ctor, we check that even an inherited ctor of a
// constexpr ctor is instantiated and have an error message issued.
namespace derived_ctor {
  namespace direct_derived_ctor {
    namespace constexpr_noninherited_ctor {
      struct foo {
	template <typename T>
	constexpr foo(T, ...) {}
      };
      struct boo : foo {
	using foo::foo;
      };
      struct bar : boo {
	template <typename ...T>
	constexpr bar(T ... args) : boo(args...) {}
      };
      void f() noexcept(noexcept(bar{0,1}));
    }

    namespace no_constexpr_noninherited_ctor {
      struct foo {
	template <typename T>
	constexpr foo(T, ...) {}
      };
      struct boo : foo {
	using foo::foo;
      };
      struct bar : boo {
	template <typename ...T>
	/* constexpr */ bar(T ... args) : boo(args...) {}
      };
      void f() noexcept(noexcept(bar{0,1}));
    }
  }

  namespace inherited_derived_ctor {
    namespace constexpr_noninherited_ctor {
      struct foo {
	template <typename T>
	constexpr foo(T, ...) {}
      };
      struct boo : foo {
	using foo::foo;
      };
      struct bor : boo {
	template <typename ...T>
	constexpr bor(T ... args) : boo(args...) {}
      };
      struct bar : bor {
	using bor::bor;
      };
      void f() noexcept(noexcept(bar{0,1}));
    }

    namespace no_constexpr_noninherited_ctor {
      struct foo {
	template <typename T>
	constexpr foo(T, ...) {}
      };
      struct boo : foo {
	using foo::foo;
      };
      struct bor : boo {
	template <typename ...T>
	/* constexpr */ bor(T ... args) : boo(args...) {}
      };
      struct bar : bor {
	using bor::bor;
      };
      void f() noexcept(noexcept(bar{0,1}));
    }
  }
}
