// PR c++/97675
// { dg-additional-options -Wno-exceptions }

struct Base { };
struct Child : Base { };
int main() {
    try { throw Child(); }
    catch (Base const&) { }
    catch (Child const&) { } // { dg-bogus "exception of type .Child. will be caught by earlier handler" }
}
