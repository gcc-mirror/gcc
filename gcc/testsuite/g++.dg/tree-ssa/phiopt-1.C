// { dg-do compile }
// { dg-options "-O -fdump-tree-phiopt1" }

#define cond_swap5(a,b);\
    t = *(a);\
    *(a) = (t<*(b))?t:*(b);\
    *(b) = (t<*(b))?*(b):t;

template<int n>
void static_sort1(int *a){
    return;
}

template<>
void static_sort1<32>(int* first){
        int t;
    static_sort1<16>(first);
    static_sort1<16>(first+16);

    cond_swap5(first + 0u, first + 16u);
    cond_swap5(first + 8u, first + 24u);
    cond_swap5(first + 8u, first + 16u);
    cond_swap5(first + 4u, first + 20u);
    cond_swap5(first + 12u, first + 28u);
    cond_swap5(first + 12u, first + 20u);
    cond_swap5(first + 4u, first + 8u);
    cond_swap5(first + 12u, first + 16u);
    cond_swap5(first + 20u, first + 24u);
    cond_swap5(first + 2u, first + 18u);
    cond_swap5(first + 10u, first + 26u);
    cond_swap5(first + 10u, first + 18u);
    cond_swap5(first + 6u, first + 22u);
    cond_swap5(first + 14u, first + 30u);
    cond_swap5(first + 14u, first + 22u);
    cond_swap5(first + 6u, first + 10u);
    cond_swap5(first + 14u, first + 18u);
    cond_swap5(first + 22u, first + 26u);
    cond_swap5(first + 2u, first + 4u);
    cond_swap5(first + 6u, first + 8u);
    cond_swap5(first + 10u, first + 12u);
    cond_swap5(first + 14u, first + 16u);
    cond_swap5(first + 18u, first + 20u);
    cond_swap5(first + 22u, first + 24u);
    cond_swap5(first + 26u, first + 28u);
    cond_swap5(first + 1u, first + 17u);
    cond_swap5(first + 9u, first + 25u);
    cond_swap5(first + 9u, first + 17u);
    cond_swap5(first + 5u, first + 21u);
    cond_swap5(first + 13u, first + 29u);
    cond_swap5(first + 13u, first + 21u);
    cond_swap5(first + 5u, first + 9u);
    cond_swap5(first + 13u, first + 17u);
    cond_swap5(first + 21u, first + 25u);
    cond_swap5(first + 3u, first + 19u);
    cond_swap5(first + 11u, first + 27u);
    cond_swap5(first + 11u, first + 19u);
    cond_swap5(first + 7u, first + 23u);
    cond_swap5(first + 15u, first + 31u);
    cond_swap5(first + 15u, first + 23u);
    cond_swap5(first + 7u, first + 11u);
    cond_swap5(first + 15u, first + 19u);
    cond_swap5(first + 23u, first + 27u);
    cond_swap5(first + 3u, first + 5u);
    cond_swap5(first + 7u, first + 9u);
    cond_swap5(first + 11u, first + 13u);
    cond_swap5(first + 15u, first + 17u);
    cond_swap5(first + 19u, first + 21u);
    cond_swap5(first + 23u, first + 25u);
    cond_swap5(first + 27u, first + 29u);
    cond_swap5(first + 1u, first + 2u);
    cond_swap5(first + 3u, first + 4u);
    cond_swap5(first + 5u, first + 6u);
    cond_swap5(first + 7u, first + 8u);
    cond_swap5(first + 9u, first + 10u);
    cond_swap5(first + 11u, first + 12u);
    cond_swap5(first + 13u, first + 14u);
    cond_swap5(first + 15u, first + 16u);
    cond_swap5(first + 17u, first + 18u);
    cond_swap5(first + 19u, first + 20u);
    cond_swap5(first + 21u, first + 22u);
    cond_swap5(first + 23u, first + 24u);
    cond_swap5(first + 25u, first + 26u);
    cond_swap5(first + 27u, first + 28u);
    cond_swap5(first + 29u, first + 30u);
};

void foo(int *a)
{
  static_sort1<32>(a);
}

// { dg-final { scan-tree-dump-not "if " "phiopt1" } }
// { dg-final { scan-tree-dump-times "MIN" 65 "phiopt1" } }
// { dg-final { scan-tree-dump-times "MAX" 65 "phiopt1" } }
