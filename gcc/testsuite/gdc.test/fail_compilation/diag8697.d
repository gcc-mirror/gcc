/*
TEST_OUTPUT:
---
fail_compilation/diag8697.d(10): Error: no property `Invalid` for type `diag8697.Base`
---
*/
interface InterBase : InterRoot { }
class Base : InterBase { }

void test(Base.Invalid) { }

interface InterRoot { }
