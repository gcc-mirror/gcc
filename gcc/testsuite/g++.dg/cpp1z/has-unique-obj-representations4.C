// PR c++/115476
// { dg-do compile { target c++11 } }

struct X;
static_assert(__has_unique_object_representations(X), "");	  // { dg-error "invalid use of incomplete type" }
static_assert(__has_unique_object_representations(X[]), "");  // { dg-error "invalid use of incomplete type" }
static_assert(__has_unique_object_representations(X[1]), "");  // { dg-error "invalid use of incomplete type" }
static_assert(__has_unique_object_representations(X[][1]), "");  // { dg-error "invalid use of incomplete type" }

struct X {
  int x;
};
static_assert(__has_unique_object_representations(X), "");
static_assert(__has_unique_object_representations(X[]), "");
static_assert(__has_unique_object_representations(X[1]), "");
static_assert(__has_unique_object_representations(X[][1]), "");
