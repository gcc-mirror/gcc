// Build don't link:
namespace X {
    template <class T> class foo;
}

template <class T>
class X::foo {
    T worthless;
};
