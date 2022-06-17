// { dg-do compile }
// { dg-require-visibility "" }

module attr_visibility2;

import gcc.attributes;

// { dg-final { scan-hidden "_D16attr_visibility25func1FZv" } }

@hidden void func1() { }

// { dg-final { scan-hidden "_D16attr_visibility25func2FZv" } }

@hidden void func2();

void func2() { }

// { dg-final { scan-hidden "_D16attr_visibility25func3FZv" } }

void func3();

@hidden void func3() { }

// { dg-final { scan-hidden "_D16attr_visibility210globalvar1i" } }

@hidden __gshared int globalvar1 = 5;
