// REQUIRED_ARGS: -w
// 4375: Dangling else

void main() {
    // disallowed
	if (true)
		foreach (i; 0 .. 5)
			if (true)
				assert(5);
    else
        assert(6);
}

