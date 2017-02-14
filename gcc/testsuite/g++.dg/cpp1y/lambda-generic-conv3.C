// PR c++/70942
// { dg-do compile { target c++14 } }

int main()
{
    int x = 0;
    [](auto&& xv){
        static_cast<decltype(xv)>(xv) = 1;
    }(x);
}
