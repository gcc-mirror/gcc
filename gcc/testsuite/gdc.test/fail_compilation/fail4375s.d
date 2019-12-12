// REQUIRED_ARGS: -w
// 4375: Dangling else

void main() {
    if (true)
        try
            assert(106);
        catch(Exception e)
            if (true)
                assert(107);
    else
        assert(108);
}

