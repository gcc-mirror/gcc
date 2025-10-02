// PR c++/99682
// { dg-additional-options "-fmodules" }
// { dg-module-cmi bar }

export module bar;
import foo;

export void bar();
