extern (C++)
{
    double twice(double d)
    {
        return d * 2;
    }

    void* createFunction(R)(R function(double));
}

void main()
{
    const f = createFunction(&twice);
}
