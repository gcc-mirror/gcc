// { dg-do compile { target c++11 } }

static_assert (1 == 0); // { dg-error "expected (string-literal|',') before" }

static_assert (1 == 0,); // { dg-error "expected string-literal before '\\)'" }

static_assert (1 == 0, "oops"); // { dg-error "static assertion failed" }
