// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

// Verify that a friend class is hidden even if it overrides a builtin
// function name.

class A {
  friend class abort;		// { dg-warning "built-in" }
  abort *b;	// { dg-error "type|expected" }
};
