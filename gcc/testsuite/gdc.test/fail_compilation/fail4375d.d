// REQUIRED_ARGS: -w
// 4375: Dangling else

void main() {
    if (true)
label2:
        if (true)
            assert(15);
    else 
        assert(16);
}

