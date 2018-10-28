// REQUIRED_ARGS: -w
// 4375: Dangling else

void main() {
    version (A)
        version (B)
            assert(25.1);
    else
        assert(25.2);
}

