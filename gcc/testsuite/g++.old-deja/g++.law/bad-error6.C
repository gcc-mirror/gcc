// { dg-do assemble  }
// GROUPS passed bad-errors
typedef __SIZE_TYPE__ size_t;

class tt {
    public:
    tt(int);

    private:
    void *operator new(size_t a); // Forbid object creation in heap memory.
};

void st(const tt&, int);

void ff(int i, int j)
{
    if( i > 0 ) {
        // This work ok.
        tt a_tt(i);
        st(a_tt, j);
    }
    else {
        // This triggers an error because of private operator new ????.
        st(tt(-i), j);
    }
}
