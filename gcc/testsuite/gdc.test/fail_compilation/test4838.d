/*
TEST_OUTPUT:
---
fail_compilation/test4838.d(13): Error: `const`/`immutable`/`shared`/`inout`/`return` attributes are only valid for non-static member functions
fail_compilation/test4838.d(14): Error: `const`/`immutable`/`shared`/`inout`/`return` attributes are only valid for non-static member functions
fail_compilation/test4838.d(15): Error: `const`/`immutable`/`shared`/`inout`/`return` attributes are only valid for non-static member functions
fail_compilation/test4838.d(16): Error: `const`/`immutable`/`shared`/`inout`/`return` attributes are only valid for non-static member functions
fail_compilation/test4838.d(17): Error: `const`/`immutable`/`shared`/`inout`/`return` attributes are only valid for non-static member functions
fail_compilation/test4838.d(18): Error: `const`/`immutable`/`shared`/`inout`/`return` attributes are only valid for non-static member functions
---
*/

void function() const fpc;
void function() immutable fpi;
void function() shared fps;
void function() shared const fpsc;
void function() inout fpw;
void function() shared inout fpsw;
