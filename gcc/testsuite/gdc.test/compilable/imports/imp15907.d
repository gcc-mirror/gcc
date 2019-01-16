module imports.imp15907;

void process(T)(T t)
{
    foreach (member; __traits(allMembers, T))
    {
        static if (__traits(getProtection, __traits(getMember, t, member)) != "private")
        {
        }
    }
}

enum allMembers(T) = [__traits(allMembers, T)];

struct PublicStruct
{
    private struct S {}
}

private:

struct PrivateStruct {}
int privateVar;
