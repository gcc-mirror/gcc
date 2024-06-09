// Verify we note all three candidates when diagnosing overload
// resolution failure.  The presence of the first two (ambiguous)
// non-strictly viable candidates used to make us prune the third
// and not note it.

void f(int, int*); // { dg-message "candidate" }
void f(int*, int); // { dg-message "candidate" }
void f(int, int, int); // { dg-message "candidate" }

int main() {
  f(1, 2); // { dg-error "no match|invalid conversion" }
}
