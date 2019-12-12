// { dg-lto-do link }
// { dg-require-effective-target lto_incremental }
// { dg-lto-options { { -flto -g1 } } }
// { dg-extra-ld-options "-r -nostdlib" }
namespace a {
    template <typename b, typename = b> class c;
    template <typename b, typename d> void e(c<b, d> &);
    void operator<<(c<char> &f, const char *) { e(f); }
    extern c<char> cout;
}
int main() { a::cout << ""; }
