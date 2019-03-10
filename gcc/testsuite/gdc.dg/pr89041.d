// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=89041
// { dg-do compile }
module pr89041;

enum dg = delegate {};

void fn()
{
    auto var = dg;

    auto inner() {
        return dg();
    }
}
