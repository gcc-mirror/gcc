// { dg-do assemble  }
namespace X {
    template <class T> class foo;
}

template <class T>
class X::foo {
    T worthless;
};
