/* Check that the conditional_trap pattern handles floating-point
   comparisons correctly.  */
void f1 (float x, float y) { if (x == y) __builtin_trap (); }
void f2 (double x, double y) { if (x == y) __builtin_trap (); }
