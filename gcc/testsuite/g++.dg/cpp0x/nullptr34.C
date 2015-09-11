// PR c++/67216
// { dg-do compile { target c++11 } }

struct s {
    s( long ) {}
};

struct t {
    t( void * ) {}
};

void foo(s) {}
void foo(t) {}

int main() {
    foo(false);
}
