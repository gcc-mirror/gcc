/*
TEST_OUTPUT:
---
fail_compilation/diag10783.d(14): Error: no property `type` for type `diag10783.Event`
fail_compilation/diag10783.d(14): Error: undefined identifier `En`
---
*/

struct Event { }

void main()
{
    Event event;
    switch (event.type) with (En)
    {
        default:
    }
}
