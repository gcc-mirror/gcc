// PR c++/94505 - bogus -Wparentheses warning with fold-expression.
// { dg-do compile { target c++17 } }
// { dg-options "-Wparentheses" }

template <bool... B>
bool foo () {
    return ((B && true) || ...); // { dg-bogus "suggest parentheses" }
}

int main () {
    foo<true, false, false, true> ();
}
