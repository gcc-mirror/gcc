// Verify -fdiagnostics-all-candidates makes us note other candidates
// when a deleted function is selected by overload resolution.
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fdiagnostics-all-candidates" }

void f(int) = delete; // { dg-message "declared here" }
void f(...); // { dg-message "candidate" }
void f(int, int); // { dg-message "candidate" }

// An example where the perfect candidate optimization causes us
// to ignore function templates.
void g(int) = delete; // { dg-message "declared here" }
template<class T> void g(T); // { dg-message "candidate" }

// An example where we have a strictly viable candidate and
// an incompletely considered bad candidate.
template<class T> void h(T, T) = delete; // { dg-message "declared here|candidate" }
void h(int*, int) = delete; // { dg-message "candidate" }

int main() {
  f(0); // { dg-error "deleted" }
  g(0); // { dg-error "deleted" }
  h(1, 1); // { dg-error "deleted" }
           // { dg-error "invalid conversion" "" { target *-*-* } .-1 } when noting 2nd cand
}
