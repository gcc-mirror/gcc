/+ TEST_OUTPUT:
---
fail_compilation/throwexp.d(11): Error: to be thrown `ret()` must be non-null
fail_compilation/throwexp.d(12): Error: to be thrown `null` must be non-null
---
+/
auto ret()
{
    return Exception.init;
}
enum y = throw ret();
enum x = throw Exception.init;
