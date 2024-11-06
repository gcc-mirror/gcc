// PR c++/113814
// { dg-additional-options "-fmodules-ts -fdump-lang-module-alias" }
// { dg-module-cmi {} }

#include "partial-6.h"
import "partial-6_a.H";

template <typename, typename = void>
struct TestTTP;

inline void test() {
  int a = f<int>().a;
  int b = g<TestTTP>().b;
}

// { dg-final { scan-lang-dump {Read:-[0-9]*'s partial merge key \(new\) template_decl:'::template A'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s partial merge key \(new\) template_decl:'::template B'} module } }

// Don't need to write the partial specialisations
// { dg-final { scan-lang-dump-not {Wrote declaration entity:[0-9]* template_decl:'::template A<#null#>'} module } }
// { dg-final { scan-lang-dump-not {Wrote declaration entity:[0-9]* template_decl:'::template B<template TT,int>'} module } }
