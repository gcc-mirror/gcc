// REQUIRED_ARGS: -vtls
// PERMUTE_ARGS:
/*
TEST_OUTPUT:
---
---
*/

template T()
{
    static this() {}
}

class C
{
    alias ti = T!();
}

void main() {}
