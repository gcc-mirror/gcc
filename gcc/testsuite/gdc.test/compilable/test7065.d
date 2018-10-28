void main()
{
    align(1)
    struct X1 { ubyte b; int n; }
    static assert(X1.sizeof == 8);
    static assert(X1.b.offsetof == 0);
    static assert(X1.n.offsetof == 4);
    //X1 x1;
    //assert(cast(void*)&x1.b == cast(void*)&x1 + 0);
    //assert(cast(void*)&x1.n == cast(void*)&x1 + 1);

    struct Y1 { ubyte b; int n; }
    static assert(Y1.sizeof == 8);
    static assert(Y1.b.offsetof == 0);
    static assert(Y1.n.offsetof == 4);
    //Y1 y1;
    //assert(cast(void*)&y1.b == cast(void*)&y1 + 0);
    //assert(cast(void*)&y1.n == cast(void*)&y1 + 4);

    int local;

    align(1)
    struct X2 { ubyte b; int n; int f(){ return local; } }
    static assert(X2.sizeof == 8 + (void*).sizeof);
    static assert(X2.b.offsetof == 0);
    static assert(X2.n.offsetof == 4);
    //X2 x2;
    //assert(cast(void*)&x2.b == cast(void*)&x2 + 0);
    //assert(cast(void*)&x2.n == cast(void*)&x2 + 1);

    struct Y2 { ubyte b; int n; int f(){ return local; } }
    static assert(Y2.sizeof == 8 + (void*).sizeof);
    static assert(Y2.b.offsetof == 0);
    static assert(Y2.n.offsetof == 4);
    //Y2 y2;
    //assert(cast(void*)&y2.b == cast(void*)&y2 + 0);
    //assert(cast(void*)&y2.n == cast(void*)&y2 + 4);
}
