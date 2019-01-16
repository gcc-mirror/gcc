// REQUIRED_ARGS: -w -profile
/*
TEST_OUTPUT:
---
---
*/

void main()
{
    string s;
    foreach (dchar c; s) // affected by dchar
        return;
}
