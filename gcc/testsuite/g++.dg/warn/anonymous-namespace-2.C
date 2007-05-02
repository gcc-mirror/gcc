// Test for the warning of exposing types from an anonymous namespace
// { dg-do compile }
//
#include "anonymous-namespace-2.h"

namespace {
    struct good { };
}

struct g1 {
    good * A;
};
struct b1 { // { dg-warning "uses the anonymous namespace" }
    bad * B;
};

struct g2 {
    good * A[1];
};
struct b2 { // { dg-warning "uses the anonymous namespace" }
    bad * B[1];
};

struct g3 {
    good (*A)[1];
};
struct b3 { // { dg-warning "uses the anonymous namespace" }
    bad (*B)[1];
};
