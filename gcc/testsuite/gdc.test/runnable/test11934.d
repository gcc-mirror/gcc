void main()
{
    struct Struct11934
    {
        this(int i) { instances++; }
        this(this) { instances++; }
        ~this() { instances--; }
        static size_t instances = 0;
    }

    struct Range11934
    {
        void popFront() { cnt++; }
        @property front() { return Struct11934(0); }
        @property empty() { return cnt >= 10; }
        size_t cnt;
    }

    foreach(ref i; Range11934()) { }

    assert(Struct11934.instances == 0);
}
