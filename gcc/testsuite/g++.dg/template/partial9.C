// PR c++/36435

template <class T> T f();
template <class T> T* f() { return 0; }

template int* f();
