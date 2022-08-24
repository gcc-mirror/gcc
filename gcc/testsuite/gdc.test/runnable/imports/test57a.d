module imports.test57a;
import imports.test57b;

// works - even fixes the error from below!
// C!(int) x;

// doesn't work
void foo() { C!(int) x; }
