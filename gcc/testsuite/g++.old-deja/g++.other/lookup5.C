// Build don't link:
// Simplified from bug report by Paris Smaragdis <paris@media.mit.edu>

// crash test - XFAIL *-*-*

template <class T> class vector {};
class foo {};
int main() {
        foo f;
        f.vector(); // ERROR - not a method
}
