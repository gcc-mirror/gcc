// Build don't link: 
// Special g++ Options: 
// GROUPS passed error-reporting
// potential bug: # line directive does not get reproduced in template
// expansion
template <class T> class A
{
public:
# 200 "lineno3.C"
      int foo () { undef1(); } // ERROR - , LINE 200
};

template class A<int>;
