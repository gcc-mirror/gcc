// PR c++/17132

template <typename T>
struct has_deref
{
    struct impl
    {
        template <
            typename Type,
            typename Type::reference (Type::*Func)(void) const>
        struct func_tag;

        template <typename Type>
        static char (& test(
            Type *,
            func_tag<Type, &Type::operator*> * = 0
        ))[2];
        static char test(void *);
    };

    static const bool value = (sizeof(impl::test((T *) 0)) == 2);
};

template <typename T>
struct container
{
    struct iterator
    {
        typedef T & reference;
        reference operator*() const;
    };
};

int main()
{
    typedef container<int>::iterator iter;
    int result = has_deref<iter>::value;
    return result;
}
