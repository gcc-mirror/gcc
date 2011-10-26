// { dg-options "-std=c++0x" }

float operator ""_abc(const char*); // { dg-error "missing space between|and suffix identifier" }
