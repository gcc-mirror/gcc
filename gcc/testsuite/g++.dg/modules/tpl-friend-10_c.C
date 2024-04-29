// PR c++/105320
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi user:part }

export module user:part;
import test_support; 
export A<int> b;
