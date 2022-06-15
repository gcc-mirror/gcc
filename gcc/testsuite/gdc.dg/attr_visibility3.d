// { dg-do compile }
// { dg-require-visibility "" }
// { dg-require-dll "" }

import gcc.attributes;

@visibility("hidden")
export void func1(); // { dg-error ".func1. was declared .export." }

@visibility("hidden")
export void func2() { } // { dg-error ".func2. was declared .export." }

@visibility("default")
export void func3();

@visibility("default")
export void func4() { };

@visibility("hidden")
export struct type1 { } // { dg-error ".type1. was declared .export." }

@visibility("default")
export struct type2 { }

@visibility("hidden")
export class type3 { } // { dg-error ".type3. was declared .export." }

@visibility("default")
export class type4 { }
