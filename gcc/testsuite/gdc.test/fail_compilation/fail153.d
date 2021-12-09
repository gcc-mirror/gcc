/*
TEST_OUTPUT:
---
fail_compilation/fail153.d(10): Error: class `fail153.Bar` cannot inherit from class `Foo` because it is `final`
---
*/

final class Foo { }

class Bar : Foo { }
