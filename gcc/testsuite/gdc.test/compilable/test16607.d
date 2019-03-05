struct A(T)
{
    T t; // causes A to be SIZEOKfwd b/c B (passed as T) isn't yet done

     // On the 2nd semantic pass through A, _scope of C got set again,
     // even though the struct was already done.
    struct C
    {
    }
}

struct B
{
    A!B* a; // causes instantiation of A!B, but can finish semantic with A!B still being fwdref
}
