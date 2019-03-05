/*
TEST_OUTPUT:
---
fail_compilation/fail35.d(15): Error: variable t cannot be read at compile time
---
*/

// http://www.digitalmars.com/d/archives/digitalmars/D/bugs/2372.html
// allegedly crashes, but cannot reproduce

void main()
{
    for (int t = 0; t < 33; t++)
    {
        size_t n = (bool[t]).sizeof;
    }
}
