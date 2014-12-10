// PR c++/63455
// { dg-options "-std=gnu++11" }

int main()
{
    int x = 0;

    // without '+0', gcc 4.6 gives a different error (no ICE though)
    decltype(({ int y = x; y; })+0) v1 = 0;
}
