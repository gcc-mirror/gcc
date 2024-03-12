/*
TEST_OUTPUT:
---
fail_compilation/diag8697.d(11): Error: no property `Invalid` for type `diag8697.Base`
fail_compilation/diag8697.d(9):        class `Base` defined here
---
*/
interface InterBase : InterRoot { }
class Base : InterBase { }

void test(Base.Invalid) { }

interface InterRoot { }
