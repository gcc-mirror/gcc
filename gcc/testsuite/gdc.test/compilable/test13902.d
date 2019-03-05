// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

void foo()
{
    int a;
    int* bar() { return &a; }
}
