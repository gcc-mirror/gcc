// PR c++/118163
// { dg-do "compile" }

template<class T>
struct S {  // { dg-note "until the closing brace" }
  S s;	    // { dg-error "has incomplete type" }
};
