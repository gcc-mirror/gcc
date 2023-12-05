// { dg-options "" }

#pragma GCC target "+sme2"

void foo() __arm_inout("zt0");
void bar() __arm_inout("za", "zt0") { foo(); } // { dg-message {call to a function that shares state other than 'za' from a function that has 'za' state} }
