// PR c++/48726
// { dg-options -std=c++0x }

#include <memory>

struct Foo{
    int i;
};
typedef std::unique_ptr<Foo> up;

std::initializer_list<up> il{up{new Foo}, up{new Foo}};
