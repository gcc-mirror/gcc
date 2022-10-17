// PR c++/93614

template<class T>
class foo{};

template<class T>
class template_class_with_struct
{
    void my_method() {
        if(this->b.foo < 1);
    };

    struct bar
    {
        long foo;
    } b;
};
