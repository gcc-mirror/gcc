// Build don't link:
// Simplified from bug report by Paris Smaragdis <paris@media.mit.edu>

template <class T> class vector {};
class foo {};
int main() {
        foo f;
        f.vector(); // gets bogus error - ICE - XFAIL *-*-*
}
