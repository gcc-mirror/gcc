// PR c++/60531

template < class T > T foo ();

bool b1 = foo<int> == foo<int>;
int (*fp1)() = +foo<int>;
