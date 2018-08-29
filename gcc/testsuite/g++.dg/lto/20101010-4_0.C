// { dg-lto-do link }
// { dg-lto-options { { -std=c++0x -flto -r -nostdlib } { -std=c++0x -flto -g -r -nostdlib } } }
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */

typedef decltype(nullptr) nullptr_t;
class shared_ptr {
public:
    shared_ptr(nullptr_t __p);
};
shared_ptr p = nullptr;
