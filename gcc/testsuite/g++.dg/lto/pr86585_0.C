// { dg-lto-do link }
// { dg-require-effective-target fpic }
// { dg-require-effective-target shared }
// { dg-lto-options { { -flto -g -nostdlib -shared -fPIC } } }
// { dg-extra-ld-options "-Wl,-undefined,dynamic_lookup -lSystem" { target *-*-darwin[12][0-9]* } }
// { dg-extra-ld-options "-Wl,-undefined,dynamic_lookup -ldylib1.o -lSystem" { target *-*-darwin[89]* } }
namespace Inkscape {
    class a;
}
class b {
    Inkscape::a *c;
    virtual void d();
};
class e {
    b f;
};
class g : e {
    void h();
};
void g::h() {}
