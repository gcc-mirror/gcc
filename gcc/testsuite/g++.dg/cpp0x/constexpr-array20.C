// PR c++/71504
// { dg-do compile { target c++11 } }

enum E { e };

constexpr bool arr[1][1] = {{true}};

template<E x, E y>
void check() { 
    static_assert(arr[x][y], ""); 
}

int main() { 
    check<e, e>(); 
}
