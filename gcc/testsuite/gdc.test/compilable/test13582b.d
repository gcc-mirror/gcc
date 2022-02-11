// REQUIRED_ARGS: -de
// EXTRA_FILES: imports/test13582.d
module test13582b;

deprecated void foo()
{
    import imports.test13582;
}

deprecated struct S
{
    import imports.test13582;
}

void main() { }
