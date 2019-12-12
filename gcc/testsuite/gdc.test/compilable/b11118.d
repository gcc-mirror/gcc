struct X(size_t Z)
{
    void set(T)(T[Z] v...)
    {
    }
}

void main()
{
    X!3 a;
    a.set(1,2,3);
}
