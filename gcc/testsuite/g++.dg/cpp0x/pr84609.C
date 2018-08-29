// PR c++/84609
// { dg-do compile { target c++11 } }

struct S {
  int s __attribute__((aligned([](char *) {})));	// { dg-error "requested alignment is not an integer constant" }
  int t [[gnu::aligned([](char *) {})]];		// { dg-error "requested alignment is not an integer constant" }
  int u __attribute__((aligned([](char *) {}))) : 2;	// { dg-error "requested alignment is not an integer constant" }
  int v [[gnu::aligned([](char *) {})]] : 4;		// { dg-error "requested alignment is not an integer constant" }
};
