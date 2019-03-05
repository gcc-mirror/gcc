
struct Contract {
    void opDispatch()(){}
}

void foo()
{
    Contract* r; if (r.empty) {}
}

