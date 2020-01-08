struct foo { short x; };

extern struct foo a; /* { dg-lto-warning {type of 'a' does not match original declaration} } */
struct foo *ptr = &a;

int main () { return 0; }
