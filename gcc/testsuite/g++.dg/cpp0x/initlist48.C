// PR c++/48726
// { dg-do compile { target c++11 } }

#include <memory>
#include <initializer_list>

struct Foo{
    int i;
};
typedef std::unique_ptr<Foo> up;

std::initializer_list<up> il{up{new Foo}, up{new Foo}};
