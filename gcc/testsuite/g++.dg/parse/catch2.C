// PR c++/62046

void foo() { } catch (...);  // { dg-error "expected" }
class bar { void foo() { } catch (...); };  // { dg-error "expected" }
