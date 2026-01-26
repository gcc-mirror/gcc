// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// From [temp.names].

using info = decltype(^^void);

template<int> struct S { };
constexpr int k = 5;
constexpr info r = ^^k;
S<[:r:]> s1;                        // { dg-error "unparenthesized splice|invalid" }
S<([:r:])> s2;                      // OK
S<[:r:] + 1> s3;                    // OK
