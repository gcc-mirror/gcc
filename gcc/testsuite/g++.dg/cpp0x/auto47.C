// PR c++/70622
// { dg-do compile { target c++11 } }

int main()
{
    auto x = 0, *y = &x;
}
