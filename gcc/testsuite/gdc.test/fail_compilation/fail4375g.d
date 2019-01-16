// REQUIRED_ARGS: -w
// 4375: Dangling else

void main() {
    static if (true)
        static if (true)
            assert(33);
    else
        assert(34);
}

