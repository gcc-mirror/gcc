// PR c++/51064
// { dg-options "-Wparentheses" }

template<int i, int j = ((i + 7) >> 3)> class foo1 { };
typedef foo1<10> bar1;

template<int i, int j = (i + 7 >> 3)> class foo2 { };
typedef foo2<10> bar2;  // { dg-warning "suggest parentheses around '\\+'" }

template<int i, int j = (100 >> (i + 2))> class foo3 { };
typedef foo3<3> bar3;

template<int i, int j = (100 >> i + 2)> class foo4 { }; 
typedef foo4<3> bar4;   // { dg-warning "suggest parentheses around '\\+'" }

template<int i, int j = (i + 7) | 3> class foo5 { };
typedef foo5<10> bar5;

template<int i, int j = i + 7 | 3> class foo6 { };
typedef foo6<10> bar6;  // { dg-warning "suggest parentheses around arithmetic" }

template<int i, int j = 3 | (i + 7)> class foo7 { };
typedef foo7<10> bar7;

template<int i, int j = 3 | i + 7> class foo8 { };
typedef foo8<10> bar8;  // { dg-warning "suggest parentheses around arithmetic" }
