// PR c++/105320
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi user }

export module user;
export import :part;
import test_support; 
A<double> c;
