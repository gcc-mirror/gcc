// { dg-lto-do link }
// { dg-require-effective-target fpic }
// { dg-require-effective-target shared }
// { dg-lto-options { { -O2 -fPIC -flto -g -shared } } }
namespace {
    class a typedef b;
    class a {};
} // namespace
class c {
    struct C {
	b d;
    };
    C e() const;
};
c::C c::e() const {
    C g;
    struct h {
	C g;
	h(C *) {}
    } f(&g);
    return g;
}

