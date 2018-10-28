// REQUIRED_ARGS: -w
// 4375: Dangling else

void main() {
    if (true)
        switch (1)  // o_O
            default:
                if (false)
                    assert(115);
    else
        assert(116);
}

