/* REQUIRED_ARGS: -o- -de
TEST_OUTPUT:
---
fail_compilation/commaexp.d(24): Deprecation: Using the result of a comma expression is deprecated
fail_compilation/commaexp.d(36): Deprecation: Using the result of a comma expression is deprecated
fail_compilation/commaexp.d(37): Deprecation: Using the result of a comma expression is deprecated
fail_compilation/commaexp.d(38): Deprecation: Using the result of a comma expression is deprecated
fail_compilation/commaexp.d(39): Deprecation: Using the result of a comma expression is deprecated
fail_compilation/commaexp.d(41): Deprecation: Using the result of a comma expression is deprecated
fail_compilation/commaexp.d(42): Deprecation: Using the result of a comma expression is deprecated
---
*/

class Entry {}
class MyContainerClass { bool append (Entry) { return false; } }

int main () {
    bool ok;
    size_t aggr;
    MyContainerClass mc;

    // Bug 15997
    enum WINHTTP_ERROR_BASE = 4200;
    enum ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED = (WINHTTP_ERROR_BASE, + 44);

    // OK
    for (size_t i; i < 5; ++i, i += 1) {}
    for (size_t i; i < 5; ++i, i += 1, i++) {}
    if (!mc)
        mc = new MyContainerClass, mc.append(new Entry);
    if (Object o = cast(Object)mc) {} // Lowering
    ok = true, mc.append(new Entry);
    assert(ok);

    // NOPE
    for (size_t i; i < 5; ++i, i += (i++, 1)) {}
    for (; aggr++, aggr > 5;) {}
    if (Object o = (ok = true, null)) {}
    ok = (true, mc.append(new Entry));
    assert(!ok);
    ok = true, (ok = (true, false));
    return 42, 0;
}
