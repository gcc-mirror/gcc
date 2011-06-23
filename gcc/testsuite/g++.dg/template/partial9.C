// PR c++/36435

template <class T> T f();
template <class T> T* f() { }

template int* f();
