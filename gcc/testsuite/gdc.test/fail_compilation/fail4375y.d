// REQUIRED_ARGS: -w
// 4375: Dangling else

static if (true)
    align(1)
        extern(C)
            pure
                static if (false)
                    void G10(){}
else
    void G11(){}

