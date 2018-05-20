// { dg-additional-options -fmodules-atom }

export module foo;
// { dg-module-bmi foo }

#pragma pack(2) // { dg-message "ended immediately before" }
int i;
