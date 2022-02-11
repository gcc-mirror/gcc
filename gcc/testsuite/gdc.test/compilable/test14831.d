// https://issues.dlang.org/show_bug.cgi?id=14831

void main()
{
    {
        int x;
        static assert(x.mangleof == "_D9test148314mainFZ1xi");
    }
    {
        int x;
        static assert(x.mangleof == "_D9test148314mainFZ4__S11xi");
    }

    {
        static int y = 0;
        static assert(y.mangleof == "_D9test148314mainFZ1yi");
    }
    {
        static int y = 0;
        static assert(y.mangleof == "_D9test148314mainFZ4__S11yi");
    }

    {
        void f() {}
        static assert(f.mangleof == "_D9test148314mainFZ1fMFNaNbNiNfZv");
    }
    {
        void f() {}
        static assert(f.mangleof == "_D9test148314mainFZ4__S11fMFNaNbNiNfZv");
    }

    {
        struct S {}
        static assert(S.mangleof == "S9test148314mainFZ1S");
    }
    {
        struct S {}
        static assert(S.mangleof == "S9test148314mainFZ4__S11S");
    }

    {
        class C {}
        static assert(C.mangleof == "C9test148314mainFZ1C");
    }
    {
        class C {}
        static assert(C.mangleof == "C9test148314mainFZ4__S11C");
    }

    {
        enum E { a }
        static assert(E.mangleof == "E9test148314mainFZ1E");
        static assert(E.a.mangleof == "_D9test148314mainFZ1E1aEQwQoFZQl");
    }
    {
        enum E { a }
        static assert(E.mangleof == "E9test148314mainFZ4__S11E");
        static assert(E.a.mangleof == "_D9test148314mainFZ4__S11E1aEQBbQuFZ4__S1Qr");
    }
}
