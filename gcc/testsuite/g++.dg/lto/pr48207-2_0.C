// { dg-lto-do link }
// { dg-lto-options { { -flto -g } } }

namespace {
    typedef struct {
	int x;
    } Foo;
}

int main () {}
