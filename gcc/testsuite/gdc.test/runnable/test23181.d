// https://issues.dlang.org/show_bug.cgi?id=23181
void main()
{
    int count;
    struct HasDtor
    {
        ~this() { ++count; }
    }

    // array[] = elem()
    // -> creates temporary to construct array and calls destructor.
    {
        count = 0;
        HasDtor[4] dtor1 = HasDtor();
        assert(count == 1);
    }
    assert(count == 5);

    // array[] = array[elem()]
    // -> constructs array using direct emplacement.
    {
        count = 0;
        HasDtor[2] dtor2 = [HasDtor(), HasDtor()];
        assert(count == 0);
    }
    assert(count == 2);
}
