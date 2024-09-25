// { dg-additional-options "-fmodules-ts" }
bool __is_constant_evaluated();
import "pr99242_a.H";
void f() { __is_constant_evaluated(); }
import "pr99242_b.H";
import "pr99242_c.H";
void g() { __is_constant_evaluated(); }
