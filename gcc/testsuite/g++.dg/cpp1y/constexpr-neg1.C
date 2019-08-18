// { dg-do compile { target c++14 } }

struct A { A(); };

constexpr int f(int i) {
  static int j = i;		// { dg-error "static" }
  thread_local int l = i;	// { dg-error "thread_local" }
  goto foo;			// { dg-error "goto" }
 foo:
  asm("foo");			// { dg-error "asm" "" { target c++17_down } }
  int k;			// { dg-error "uninitialized" }
  A a;				// { dg-error "non-literal" }
  return i;
}

// FIXME remove
// { dg-prune-output "return" }
