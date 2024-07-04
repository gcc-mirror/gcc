// { dg-do compile { target c++11 } }
// Negative tests.

struct [[no_dangling]] A {	    // { dg-warning "ignored" }
 [[no_dangling]] int &foo (int &);   // { dg-warning "ignored" }
};

[[no_dangling]] int &bar (int &);    // { dg-warning "ignored" }

[[gnu::no_dangling]] int i;	    // { dg-warning "ignored" }
[[gnu::no_dangling]] double d;	    // { dg-warning "ignored" }
[[gnu::no_dangling]] typedef int T;  // { dg-warning "ignored" }

[[gnu::no_dangling()]] int &fn1 (int &);	    // { dg-error "parentheses" }
[[gnu::no_dangling("a")]] int &fn2 (int &);  // { dg-error "must be an expression" }
[[gnu::no_dangling(true, true)]] int &fn3 (int &);  // { dg-error "wrong number of arguments" }

enum [[gnu::no_dangling]] E {	    // { dg-warning "ignored" }
  X [[gnu::no_dangling]]		    // { dg-warning "ignored" }
};

[[gnu::no_dangling]];		    // { dg-warning "ignored" }

void
g ()
{
  goto L;
[[gnu::no_dangling]] L:;		    // { dg-warning "ignored" }
}
