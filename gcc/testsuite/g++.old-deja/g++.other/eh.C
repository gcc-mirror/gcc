// { dg-do assemble  }

class foo {
public:
    ~foo();
    foo &operator=(const foo &);
    foo	e() const;
};
class bar {
public:
    foo	d() const;
};
struct x {
    foo a;
    bar* b;
};
void baz(x *f, int ic)
{
    f->a = ic ? f->b->d().e() : f->b->d();
}
