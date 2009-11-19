// { dg-lto-do link }
// { dg-lto-options {{-fPIC -fwhopr}} }
// { dg-extra-ld-options "-fPIC -fwhopr -r -nostdlib -fno-exceptions" }
void func(); class Foo { };
void bar() { try { func(); } catch (Foo) { } };
