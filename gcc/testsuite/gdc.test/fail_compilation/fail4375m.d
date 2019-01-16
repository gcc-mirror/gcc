// REQUIRED_ARGS: -w
// 4375: Dangling else

void main() {
    do
        if (true)
            if (true)
                assert(76);
        else
            assert(77);
    while (false);
}

