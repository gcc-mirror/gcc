// PR c++/97675

struct Base { };
struct Child : Base { };
int main() {
    try { throw Child(); }
    catch (Base const&) { }
    catch (Child const&) { } // { dg-warning "exception of type .Child. will be caught by earlier handler" }
}
