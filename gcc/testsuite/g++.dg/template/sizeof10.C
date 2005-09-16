// PR c++/23357

template<typename T> bool foo()
{
    const long int i = sizeof(T) > 1 ? sizeof(T) : 0;
    return i > 0;
}
