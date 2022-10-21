// PR c++/102963
// { dg-additional-options "-fmodules-ts -fconcepts" }
// { dg-module-cmi pr102963 }

export module pr102963;

export template<class T> concept C = __is_same(T, int);
