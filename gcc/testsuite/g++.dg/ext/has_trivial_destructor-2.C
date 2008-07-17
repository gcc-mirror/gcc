// PR c++/36855

typedef char assert_0 [__has_trivial_destructor (int&) ? 1 : -1];
