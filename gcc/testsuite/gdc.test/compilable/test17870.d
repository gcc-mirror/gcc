alias AliasSeq(T...) = T;

class A
{
    int z = 3;
}

class B : A
{
    int a = 1;
}

class C : B
{
    int b = 2;
    alias tup = AliasSeq!(b, a, z);
}

void main()
{
    static const ins = new C;
    static assert(&ins.tup[0] == &ins.b);
    static assert(&ins.tup[1] == &ins.a);
    static assert(&ins.tup[2] == &ins.z);
    static assert(ins.tup == AliasSeq!(2,1,3));
}
