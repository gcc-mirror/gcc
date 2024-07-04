// PR c++/97675

// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

struct Base { };
struct Child : Base { };
int main() {
    try { throw Child(); }
    catch (Base const&) { }
    catch (Child const&) { } // { dg-warning "exception of type .Child. will be caught by earlier handler" }
}
