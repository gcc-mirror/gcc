// { dg-do compile }

// PR c++/3663
// Enforce access of nested type.

template <typename A>
class S {
  class T {};				// { dg-message "private" }
};

template <typename A>
typename A::T* f (A) {			// { dg-error "this context" }
  return 0;
}

void g () {
  f (S<int> ());			// { dg-message "required|no match" }
}
