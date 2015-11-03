// PR c++/67845

typedef void F () const;

F foo;  // { dg-error "cv-qualifier" }
void foo ();
