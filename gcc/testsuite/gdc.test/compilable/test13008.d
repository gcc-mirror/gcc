// REQUIRED_ARGS: -o-
// PERMUTE_ARGS: -d -de -dw
/*
TEST_OUTPUT*
---
---
*/
deprecated class Dep { }
deprecated Dep depFunc1(); // error
deprecated void depFunc2(Dep); // error
