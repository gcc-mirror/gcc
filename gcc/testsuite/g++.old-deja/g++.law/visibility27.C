// Build don't link: 
// GROUPS passed visibility
// visibility file
// From: jbuck@synopsys.com (Joe Buck)
// Date:     3 Aug 1994 01:52:04 GMT
// Subject:  2.6.0 bug with protected members and virtual baseclasses
// Message-ID: <31mt84$lfq@hermes.synopsys.com>

struct R {
protected:
        virtual void foo();
};

struct A : public R {
};

struct B : virtual public A {
        void bletch() { foo();}
};
