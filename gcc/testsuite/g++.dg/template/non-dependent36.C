// PR c++/123495

template <int N>
void foo () { ((int (*) ()) 0) (); }
