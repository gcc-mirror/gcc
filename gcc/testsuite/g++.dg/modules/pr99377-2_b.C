// PR c++/99377
// { dg-additional-options -fmodules-ts }
// { dg-do link }
import pr99377_2;

template<class> void g() { f(); }

int main() { f(); }
