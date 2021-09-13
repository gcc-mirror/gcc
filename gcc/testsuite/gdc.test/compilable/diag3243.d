// REQUIRED_ARGS: -vtls
// PERMUTE_ARGS:
template T()
{
    static this() {}
}

class C
{
    alias ti = T!();
}

void main() {}
