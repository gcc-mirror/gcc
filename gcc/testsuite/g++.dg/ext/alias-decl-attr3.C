// { dg-do run { target c++11 } }

template <class T>
int
align_of_type_wide_array()
{
    using type_wide_array __attribute((aligned(__alignof(T))))
        = unsigned char[sizeof (T)];

    return __alignof(type_wide_array);
}

int
main ()
{
    if (align_of_type_wide_array<int>() == __alignof(int))
        return 0;
    else
        return 1;
}
