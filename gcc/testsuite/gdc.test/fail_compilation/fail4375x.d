// REQUIRED_ARGS: -w
// 4375: Dangling else

static if (true)
abstract:
    static if (false)
        class G5 {}
else
    class G6 {}

