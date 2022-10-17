void main()
{
    struct  A;
    struct  B { struct CD;}
    alias   V = void;
    alias   I = int;
    V    test0(@A I)                    {}
    V    test1(@A I p)                  {}
    V    test2(@A @(B) I)               {}
    V    test3(@(B.CD) @B I)            {}
    V    test4(@A I, @B @A I)           {}
    V    test5(@A I p, @(B.CD) @A I )   {}
}
