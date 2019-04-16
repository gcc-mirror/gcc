// { dg-lto-do link }
// { dg-lto-options { { -O2 -fPIC -flto } } }
// { dg-require-effective-target shared }
// { dg-require-effective-target fpic }
// { dg-extra-ld-options "-shared -g" }

class a {};
class b : virtual a {
public:
    void operator<<(bool);
};
void c() try {
    b d;
    d << "";
} catch (int) {
}
