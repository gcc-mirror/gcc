// { dg-do preprocess  }
// { dg-options "-std=c++11" { target c++ } }
// { dg-options "-std=gnu99" { target c } }
// { dg-additional-options -fdirectives-only }

R"stuff(
)nope"
#error in raw literal
)stuff"
// comment
#define bob 1
// " comment
#if !bob
#error "no bob"
#endif

/* comment */
