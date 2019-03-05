// REQUIRED_ARGS: -w
// 4375: Dangling else

void main() {
    version (A)
        if (true)
            assert(24);
    else
        assert(25);
}

