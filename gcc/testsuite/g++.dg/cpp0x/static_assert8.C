// { dg-do compile { target c++11 } }

static_assert (1 == 0); // { dg-error "static assertion failed" }

static_assert (1 == 0,); // { dg-error "expected string-literal before '\\)'" }

static_assert (1 == 0, "oops"); // { dg-error "static assertion failed" }

// { dg-error "static_assert without a message only available with " "" { target { ! c++17 } } 3 }
