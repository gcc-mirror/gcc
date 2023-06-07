// { dg-lto-do link }
// Explicit { dg-require-effective-target exceptions_enabled } so that dependent tests don't turn UNRESOLVED for '-fno-exceptions'.
// { dg-require-effective-target fpic }
// { dg-lto-options {{-fPIC -flto -flto-partition=1to1}} }
// { dg-extra-ld-options "-fPIC -flto -flto-partition=1to1 -r -fno-exceptions -flinker-output=nolto-rel" }
void func(); class Foo { };
void bar() { try { func(); } catch (Foo) { } };
