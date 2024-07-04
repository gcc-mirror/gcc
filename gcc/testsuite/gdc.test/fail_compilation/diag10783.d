/*
TEST_OUTPUT:
---
fail_compilation/diag10783.d(15): Error: no property `type` for `event` of type `diag10783.Event`
fail_compilation/diag10783.d(10):        struct `Event` defined here
fail_compilation/diag10783.d(15): Error: undefined identifier `En`
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
