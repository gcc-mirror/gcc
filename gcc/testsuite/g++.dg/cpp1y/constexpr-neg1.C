// { dg-do compile { target c++14 } }

struct A { A(); };

constexpr int f(int i) {
  static int j = i;		// { dg-error "static" }
  thread_local int l = i;	// { dg-error "thread_local" "" { target c++20_down } }
  goto foo;			// { dg-error "goto" "" { target c++20_down } }
 foo:
  asm("foo");			// { dg-error "asm" "" { target c++17_down } }
  int k;			// { dg-error "uninitialized" "" { target c++17_down } }
  A a;				// { dg-error "non-literal" "" { target c++20_down } }
  return i;
}

// FIXME remove
// { dg-prune-output "return" }
