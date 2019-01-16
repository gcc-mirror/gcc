// REQUIRED_ARGS: -w
// 4375: Dangling else

static if (true)
    static if (false)
        struct G1 {}
else
    struct G2 {}

