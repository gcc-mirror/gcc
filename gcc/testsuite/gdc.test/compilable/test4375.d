// REQUIRED_ARGS: -unittest
// 4375: disallow dangling else

void main() {

	if (true) {
		if (false) {
			assert(1);
		} else {
			assert(2);
		}
	}


    if (true) {
        if (false)
            assert(7);
    } else
        assert(8);


    if (true) {
        if (false)
            assert(9);
        else
            assert(10);
    }


    {
        if (true)
            assert(11);
        else
            assert(12);
    }


    {
label1:
        if (true)
            assert(13);
        else
            assert(14);
    }


	if (true)
		foreach (i; 0 .. 5) {
			if (true)
				assert(17);
            else
                assert(18);
        }


	if (true) {
		foreach (i; 0 .. 5)
			if (true)
				assert(18.1);
    } else
        assert(18.2);


    if (true)
        assert(19);
    else
        assert(20);


    if (true)
        assert(21);
    else if (false)
        assert(22);
    else
        assert(23);


    version (A) {
        if (true)
            assert(26);
    } else
        assert(27);


    version (A) {
        if (true)
            assert(28);
        else
            assert(29);
    }


    version (A)
        assert(30);
    else version (B)
        assert(31);
    else
        assert(32);


    static if (true) {
        static if (true)
            assert(35);
    } else
        assert(36);


    static if (true) {
        static if (true)
            assert(37);
        else
            assert(38);
    }


    static if (true)
        assert(39);
    else static if (true)
        assert(40);
    else
        assert(41);

    switch (4) {
        case 0:
            if (true)
                assert(42);
            else
                assert(43);
            break;
        case 1: .. case 5:
            if (true)
                assert(44);
            else
                assert(45);
            break;
        default:
            if (true)
                assert(46);
            else
                assert(47);
            break;
    }

    // (o_O)
    switch (1)
        default:
            if (true)
                assert(113);
            else
                assert(114);

    // (o_O)
    final switch (1)
        case 1:
            if (true)
                assert(117);
            else
                assert(118);

    mixin(q{
        if (true)
            assert(56);
        else
            assert(57);
    });



    while (false)
        if (true)
            assert(66);
        else
            assert(67);


    if (true)
        while (false)
            assert(68);
    else
        assert(69);


    do
        if (true)
            assert(72);
        else
            assert(73);
    while (false);


    if (true)
        do
            if (true)
                assert(74);
            else
                assert(75);
        while (false);

    for (
        if (true)        // (o_O)
            assert(78);
        else
            assert(79);
        false; false
    )
        if (true)
            assert(80);
        else
            assert(81);

    if (true)
        for (if (true) assert(84); else assert(85); false;)
            assert(86);


    if (true)
        if (true)
            if (true)
                if (true)
                    if (true)
                        assert(87);

    auto x = new C;


    if (true)
        while (false)
            for (;;)
                scope (exit)
                    synchronized (x)
                        assert(88);
    else
        assert(89);


    if (true)
        while (false)
            for (;;) {
                scope (exit)
                    synchronized (x)
                        if (true)
                            assert(90);
                        else
                            assert(89);
            }


    if (true)
        while (false)
            for (;;)
                scope (exit)
                    synchronized (x)
                        if (true)
                            assert(90);
                        else
                            assert(89);
    else
        assert(12); 


    with (x)
        if (false)
            assert(92);
        else
            assert(93);


    try
        if (true)
            assert(94);
        else
            assert(95);
    catch (Exception e)
        if (true)
            assert(96);
        else
            assert(97);
    finally
        if (true)
            assert(98);
        else
            assert(99);


    if (true)
        try
            if (true)
                assert(100);
            else
                assert(101);
        finally
            assert(102);

    if (true)
        try
            assert(109);
        catch(Exception e)
            if (true)
                assert(110);
            else
                assert(112);                
        finally
            assert(111);

    static struct F {
        static if (true)
            int x;
        else
            int y;

        static if (true) {
            static if (false)
                int z;
        } else
            int w;

        static if (true)
            int t; 
        else static if (false)
            int u;
        else
            int v;
    }

    if (true)
        if (true)
            assert(113);
        else
            assert(114);
    else
        assert(115);

    static if (true)
        static if (true)
            assert(116);
        else
            assert(117);
    else
        assert(118);

}

unittest {
    if (true)
        assert(50);
    else
        assert(51);
}

class C {
    invariant() {
        if (true)
            assert(58);
        else
            assert(59);
    }

    int f()
    in {
        if (true)
            assert(60);
        else
            assert(61);
    }
    out(res) {
        if (true)
            assert(62);
        else
            assert(63);
    }
    body {
        if (true)
            assert(64);
        else
            assert(65);
        return 0;
    }
}

enum q = q{
    if(true)
        if(true)
            assert(54.1);
    else
        assert(55.2);
};

static if (true)
    struct F0 {}
else static if (true)
    struct F1 {}
else
    struct F2 {}

static if (true) {
    static if (false)
        struct F3 {}
} else
    struct F4 {}

version(A) {
    version(B)
        struct F5 {}
} else
    struct F6 {}

version(A) {
    version(B)
        struct F5a {}
    else
        struct F5b {}
}

version (C)
    struct F5c {}
else
    struct F5d {}

struct F7 {
    static if (true)
        int x;
    else
        float x;

private:
    static if (true)
        int y;
    else
        float y;
}

template F8() {
    static if (true)
        int x;
    else
        float x;
}

static if (true)
    align(1)
        static if (false)
            struct F9 {}

static if (true)
    align(1) {
        extern(C)
            pure
                static if (false)
                    void F10(){}
                else
                    void F11(){}
    }


void f() {
    int[] x;
    static if (5 > 0)
        version (Y)
            scope (failure)
                foreach (i, e; x)
                    while (i > 20)
                        with (e)
                            if (e < 0)
                                synchronized(e)
                                    assert(1);
                            else
                                assert(2);
        else
            x = null;
    else
        x = null;
}

