// Build don't link:
// Origin: Joe Buck <jbuck@welsh-buck.org>

#include <vector>
using std::vector;

struct foo {
int a;
};

bool operator==(const foo&, const foo&);

bool veq(const vector<foo>& a, const vector<foo>& b)
{
return a == b;
}
