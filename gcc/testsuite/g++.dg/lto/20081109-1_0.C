// { dg-lto-do link }
// { dg-lto-options {{-fPIC -fwhopr}} }
// { dg-extra-ld-options "-fPIC -fwhopr -shared -fno-exceptions" }
void func(); class Foo { };
void bar() { try { func(); } catch (Foo) { } };
