/*
TEST_OUTPUT:
---
fail_compilation/foreach.d(12): Error: cannot declare `out` loop variable, use `ref` instead
fail_compilation/foreach.d(13): Error: cannot declare `out` loop variable, use `ref` instead
fail_compilation/foreach.d(13): Error: cannot declare `out` loop variable, use `ref` instead
---
*/
void main ()
{
    int[] array;
    foreach (out val; array) {}
    foreach (out idx, out val; array) {}
}
