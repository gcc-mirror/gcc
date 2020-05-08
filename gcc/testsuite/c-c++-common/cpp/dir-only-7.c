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

bob\
\
R"regular string not an erroneous raw one"

"regular"R"***(not a raw string"
#define HERE 1
 //)***"
#ifndef HERE
#error "oops no HERE"
#endif
 /* comment */


0e+R"*(not a raw string"
#define CPP_NUM 1
 //)*"
#ifndef CPP_NUM
#error "oops no CPP_NUM"
#endif
