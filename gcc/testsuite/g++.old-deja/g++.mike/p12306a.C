// prms-id: 12306
// a net report of the same problem as 12306

class a {
public:
    int i;
};

class g : virtual public a {
};

class b : virtual public a {
    int j;
};

class c : public g, public b {
};

class d {
public:
    virtual class b* get() {return 0;}
};

class f : public d {
public:
    virtual class b* get() {return &_c;}
    c _c;
};

int main(void) {
    f D;
    b* bp=D.get();
    D._c.i = 42;
    return &D._c.i != &bp->i;
}
