// PR c++/107532
// { dg-do compile { target c++11 } }
// { dg-options "-Wdangling-reference" }

struct R
{
    int& r;
    int& get() { return r; }
    int&& rget() { return static_cast<int&&>(r); }
};

int main()
{
    int i = 42;
    int& l = R{i}.get(); // { dg-bogus "dangling reference" }
    int const& cl = R{i}.get(); // { dg-bogus "dangling reference" }
    int&& r = R{i}.rget(); // { dg-bogus "dangling reference" }
    int const&& cr = R{i}.rget(); // { dg-bogus "dangling reference" }
    (void) l;
    (void) r;
    (void) cr;
    (void) cl;
}
