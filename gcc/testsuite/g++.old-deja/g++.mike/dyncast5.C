#include <stddef.h>

void *p;
int fail;

class HeapTracked {
public:
    virtual ~HeapTracked() = 0;
    void *operator new(size_t size);
    void operator delete(void *ptr);
    static bool isObjectAllocation(const HeapTracked *ptr);
};

HeapTracked::~HeapTracked(){}
void * HeapTracked::operator new(size_t size)
{
    void * memPtr = ::operator new(size);
    p = memPtr;
    return memPtr;
}

void HeapTracked::operator delete(void *ptr)
{
    if (p != ptr)
      fail = 1;
    ::operator delete(ptr);
}

bool HeapTracked::isObjectAllocation(const HeapTracked *ptr)
{
    if (p != const_cast<void*>(dynamic_cast<const void*>(ptr)))
      fail = 1;
    return false;
}

class Mumble1: public virtual HeapTracked {
    double d;
public:
    virtual ~Mumble1(){}
};

class Mumble2: public virtual HeapTracked {
    double d;
public:
    virtual ~Mumble2(){}
};

class Foo: virtual public HeapTracked,
           virtual public Mumble1,
           virtual public Mumble2 {
public:
    ~Foo(){}
};

int main()
{
    Foo *pf = new Foo;
    pf->isObjectAllocation(pf);

    Mumble1 *pm1 = pf;
    pm1->isObjectAllocation(pm1);

    Mumble2 *pm2 = pf;
    pm2->isObjectAllocation(pm2);

    // delete pf;
    // delete pm1;
    delete pm2;

    return fail;
}
