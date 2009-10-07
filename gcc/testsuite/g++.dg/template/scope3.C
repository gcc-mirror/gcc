// PR c++/41038

struct S
{
    int size() const;
};

template<typename T>
struct Packer
{
    int foo() {
        return  Packer::var.size();
    }
    const S& var;
};
