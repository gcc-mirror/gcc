// PR c++/109640
// { dg-do compile { target c++11 } }
// { dg-options "-Wdangling-reference" }

bool
fn0 ()
{
    int a;
    int&& i = [](int& r) -> int&& { return static_cast<int&&>(r); }(a); // { dg-bogus "dangling reference" }
    auto const l = [](int& r) -> int&& { return static_cast<int&&>(r); };
    int&& j = l(a);
    return &i == &j;
}
