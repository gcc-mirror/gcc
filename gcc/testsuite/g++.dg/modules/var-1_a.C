// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

export module Var;
// { dg-module-bmi Var }

export int counter = 2;
export extern const int limit = 5;

