// Test that we track line numbers properly across newlines
// both escaped and not in raw strings.
// { dg-options "-std=gnu99" { target c } }
// { dg-options "-std=c++0x" { target c++ } }

const char a[] = R"(\

)";

T t;				// { dg-error "" }
