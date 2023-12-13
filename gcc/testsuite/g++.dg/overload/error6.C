// Verify we note even non-template candidates when diagnosing
// overload resolution failure for a template-id.

template<class T> void f(T); // { dg-message "candidate" }
void f(int); // { dg-message {candidate: 'void f\(int\)' \(ignored\)} }

int main() {
  f<int>(0, 0); // { dg-error "no match" }
}
