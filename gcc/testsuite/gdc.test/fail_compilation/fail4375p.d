// REQUIRED_ARGS: -w
// 4375: Dangling else

void main() {
    if (true)
        while (false)
            for (;;)
                scope (exit)
                    synchronized (x)
                        if (true)
                            assert(90);
    else
        assert(89);
}

