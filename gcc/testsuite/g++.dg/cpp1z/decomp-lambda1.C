// PR c++/84420
// { dg-do compile { target c++17 } }

int main(){
    int a[1]{};
    [&a]{
        auto [v] = a;
        (void)v;
    }();
}
