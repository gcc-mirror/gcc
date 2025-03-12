/*
TEST_OUTPUT:
---
fail_compilation/fail20616.d(16): Error: undefined identifier `$`
fail_compilation/fail20616.d(13):        perhaps define `opDollar` for `X`
fail_compilation/fail20616.d(18): Error: undefined identifier `$`
fail_compilation/fail20616.d(13):        perhaps define `opDollar` for `X`
---
*/
module fail20616;

void g() {
    struct X {
        auto opSlice(size_t a, size_t b) { return ""; }
    }
    auto x = X()[0 .. $];
    auto b = X();
    auto c = b[0 .. $ - 1];
    auto v = [1, 2, 3];
    auto d = v[$.. $];
}

int main() {
    g();
    return 0;
}
