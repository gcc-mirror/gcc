// PR c++/15542

template <typename> struct S_T { 
  const char** operator & (); 
}; 
 
template <class T> void foo(T **) {} 
 
template <typename> void templateTest() { 
  S_T<const char> s_t; 
  foo(&s_t); 
} 
