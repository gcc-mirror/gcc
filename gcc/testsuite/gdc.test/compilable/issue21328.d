
static foreach(i; 1 .. 5)
void foo(float[i] arr)
{
    () {
        float x = arr[0];
    } ();
}
