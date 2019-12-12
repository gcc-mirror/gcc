// REQUIRED_ARGS: -w
// 4375: Dangling else

void main() {
    if (true)
        while (false)
            if (true)
                assert(70);
    else
        assert(71);
}

