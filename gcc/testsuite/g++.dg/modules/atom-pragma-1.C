// { dg-additional-options -fmodules-atom }

export module foo;
// { dg-module-bmi foo }

#pragma bob // { dg-message "ended immediately before" }
int i;
