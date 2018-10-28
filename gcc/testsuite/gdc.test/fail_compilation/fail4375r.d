// REQUIRED_ARGS: -w
// 4375: Dangling else

void main() {
    if (true)
        try
            assert(103);
        finally
            if (true)
                assert(104);
    else
        assert(105);
}

