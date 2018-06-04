// { dg-lto-do link }
// { dg-require-effective-target fpic }
// { dg-lto-options {{-fPIC -flto -flto-partition=1to1}} }
// { dg-extra-ld-options "-fPIC -flto -flto-partition=1to1 -r -nostdlib -fno-exceptions -flinker-output=nolto-rel" }
void func(); class Foo { };
void bar() { try { func(); } catch (Foo) { } };
