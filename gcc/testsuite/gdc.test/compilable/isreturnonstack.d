
struct S { int[10] a; }
int test1();
S test2();

static assert(__traits(isReturnOnStack, test1) == false);
static assert(__traits(isReturnOnStack, test2) == true);
