// PR c++/71238

int main()
{
    int x=myFunc(3234);  // { dg-error "11:'myFunc' was not declared" }
}
