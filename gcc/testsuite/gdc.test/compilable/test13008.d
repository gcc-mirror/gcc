// REQUIRED_ARGS: -o-
// PERMUTE_ARGS: -d -de -dw

deprecated class Dep { }
deprecated Dep depFunc1(); // error
deprecated void depFunc2(Dep); // error
