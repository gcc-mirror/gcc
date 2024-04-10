template<class T>
struct List
{
    T* begin;
};

struct StructWithDestructor
{
    ~StructWithDestructor();

    int i;
};

struct StructWithCopyCtor
{
    StructWithCopyCtor();
    StructWithCopyCtor(const StructWithCopyCtor &other);

    int i;
};

StructWithDestructor::~StructWithDestructor()
{
}

StructWithCopyCtor::StructWithCopyCtor()
{
}

StructWithCopyCtor::StructWithCopyCtor(const StructWithCopyCtor &other) : i(other.i)
{
}

StructWithDestructor getStructWithDestructor()
{
    StructWithDestructor r;
    r.i = 12345;
    return r;
}

StructWithCopyCtor getStructWithCopyCtor()
{
    StructWithCopyCtor r;
    r.i = 54321;
    return r;
}
