struct MaybeShared
{
    this(this T)()
    {

    }

    ~this()
    {

    }
}

void main() {
    {
        auto aboutToDie = MaybeShared();
    }
    {
        auto aboutToDie = shared MaybeShared();
    }
}
