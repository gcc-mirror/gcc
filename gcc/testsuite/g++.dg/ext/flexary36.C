/* PR c++/93753 - ICE on a flexible array followed by a member in
   an anonymous struct with an initializer
   { dg-do compile }
   { dg-options "-Wall -Wno-unused-variable" } */

struct {
  int a[];          // { dg-error "flexible array member '<unnamed struct>::a' not at end of 'struct<unnamed>'" }
  int b;
} ax;

struct {
  int a[];          // { dg-error "flexible array member '<unnamed struct>::a' not at end of 'struct<unnamed>'" }
  int b;
} bx = { };

struct {
  int a[];          // { dg-error "flexible array member '<unnamed struct>::a' not at end of 'struct<unnamed>'" }
  int b;
} cx = { 0 };

struct {
  int a[];          // { dg-error "flexible array member '<unnamed struct>::a' not at end of 'struct<unnamed>'" }
  int b;
} dx = { 1 };


union {
  int a[];
  int b;
} du = { 1 };


struct A {
  int a[];          // { dg-error "flexible array member 'A::a' not at end of 'struct A'" }
  int b;
} a;

struct B {
  int a[];          // { dg-error "flexible array member 'B::a' not at end of 'struct B'" }
  int b;
} b = { };

struct C {
  int a[];          // { dg-error "flexible array member 'C::a' not at end of 'struct C'" }
  int b;
} c = { 0 };

struct D {
  int a[];          // { dg-error "flexible array member 'D::a' not at end of 'struct D'" }
  int b;
} d = { 1 };


struct E {
  struct {
    int a[];        // { dg-error " not at end " }
    int b;
  } e = { 1 };      // { dg-error "non-static initialization of a flexible array member" }
};

struct G {
  struct {
    int a[];        // { dg-error " not at end " }
    int b;
  };
} g = { 1 };        // { dg-error "initialization of flexible array member in a nested context" }

struct H {
  int i;
  struct {
    int a[];        // { dg-error " not at end " }
    int b;
  };
} h = { 1 };

namespace {

struct {
  int a[];          // { dg-error " not at end of " }
  int b;
} ax;

struct {
  int a[];          // { dg-error " not at end " }
  int b;
} bx = { };

struct {
  int a[];          // { dg-error " not at end " }
  int b;
} cx = { 0 };

struct {
  int a[];          // { dg-error " not at end " }
  int b;
} dx = { 1 };


struct A {
  int a[];          // { dg-error " not at end of 'struct {anonymous}::A'" }
  int b;
} a;

struct B {
  int a[];          // { dg-error " not at end of 'struct {anonymous}::B'" }
  int b;
} b = { };

struct C {
  int a[];          // { dg-error " not at end of 'struct {anonymous}::C'" }
  int b;
} c = { 0 };

struct D {
  int a[];          // { dg-error " not at end of 'struct {anonymous}::D'" }
  int b;
} d = { 1 };

}

// { dg-prune-output "unnamed type with no linkage used to declare variable" }
// { dg-prune-output "non-static data member initializers" }
// { dg-prune-output "extended initializer lists" }
