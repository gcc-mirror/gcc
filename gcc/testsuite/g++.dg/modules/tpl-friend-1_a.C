// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks" }
// declarations followed by friend injection

export module foo;
// { dg-module-bmi foo }

void foo (int, void *);
void foo (float, void *);

template <typename T> class TPL
{
  friend void foo (T, void *); // { dg-warning "non-template function" }

  T member;
};

template class TPL<float>;  // instantiate

// binding->465500
// FUNCTION_DECL->465500
//   DECL_TEMPLATE_INFO->NULL
//   DECL_USE_TEMPLATE->0

// specialization 465500
// tmpl->333580 template_decl
//  the friend decl implicit template

// args->46b640 tree_vec
//   length:1
//   elt:0 real_type

// do not add this (non-)specialization to the depset table
// the ::foo fns and TPL should be in different depsets
// the friend decl should be streamed as part of TPL's definition

// { dg-final { scan-lang-dump-not {Connecting declaration decl template_decl:'::foo@foo:1'} module } }

// { dg-final { scan-lang-dump {Cluster:1 3 depsets\n  \[0\]=decl declaration '::foo@foo:1'\n  \[1\]=decl declaration '::foo@foo:1'\n  \[2\]=binding '::foo'} module } }
// { dg-final { scan-lang-dump {Cluster:2 2 depsets\n  \[0\]=decl definition '::TPL@foo:1'\n  \[1\]=binding '::TPL'\n} module } }
// { dg-final { scan-lang-dump {Cluster:3 . depsets\n  \[0\]=specialization definition '::TPL@foo:1<float>'} module } }
