/*
TEST_OUTPUT:
---
fail_compilation/ice13563.d(23): Error: undefined identifier `z` in module `ice13563`
---
*/

struct Payload
{
    void opIndex(K)(K i) {}
    void opIndexAssign(T, N)(T value, N i) {}
}

struct Value
{
    Payload payload;
    alias payload this;
}

void main()
{
    Value v;
    v["name"] = .z();           // ICE
  //v["name"] = z();            // OK
  //v.opIndex("name") = .z();   // OK
  //v.payload["name"] = .z();   // OK
}
