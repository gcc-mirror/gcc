// { dg-do compile }

bool f();

struct counted_base {
    virtual void destroy() { }
    void release() { if (f()) destroy(); }
};

struct shared_count {
    shared_count() { }
    ~shared_count() { if (pi) pi->release(); }
    shared_count(shared_count& r) : pi(r.pi) { if (pi) pi->release(); }
    counted_base* pi;
};

struct Foo;

struct shared_ptr  {
    Foo& operator*() { return *ptr; }
    Foo* ptr;
    shared_count refcount;
};

struct Bar {
    Bar(Foo&, shared_ptr);
};

void g() {
    shared_ptr foo;
    new Bar(*foo, foo);
}

// { dg-final { cleanup-tree-dump "vect" } }
