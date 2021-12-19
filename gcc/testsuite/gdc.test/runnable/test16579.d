// REQUIRED_ARGS: -unittest
// PERMUTE_ARGS:
// https://issues.dlang.org/show_bug.cgi?id=16579

struct Thing
{
    enum Instance = Thing();
    int a = 42;

    void iter()
    {
        assert(this.a == 42);
    }
}

void main()
{
    return Thing.Instance.iter;   // Added 'return'
}

// From https://issues.dlang.org/show_bug.cgi?id=16576

alias a = test2!();
alias b = test3!();


template test2()
{
    struct Thing{
        static enum Instance = Thing([0, 1, 2, 3]);
        int[] array;
        void iter(in string str) const{
            foreach(j, tup; this.array) assert(tup == j);
            assert(this.array && this.array.length == 4);
        }
    }
    unittest{
        auto test(in string str){return Thing.Instance.iter(str);}
        test("?");
    }
}

template test3()
{
    struct Thing{
        static enum Instance = Thing([0, 1, 2, 3]);
        int[] array;
        void iter() const{
            foreach(j, tup; this.array) assert(tup == j);
            assert(this.array && this.array.length == 4);
        }
    }
    unittest{
        auto test(){return Thing.Instance.iter();}
        test();
    }
}
