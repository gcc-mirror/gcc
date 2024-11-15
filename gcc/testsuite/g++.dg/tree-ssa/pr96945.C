// { dg-do compile { target c++11 } }
// { dg-options "-O1 -fdump-tree-optimized" }
#include <vector>
struct c {
    c() = default;
    c(const c&) =default;
    c(c&&) = default;
};
void foo(){
    std::vector<c> vi = {c(),c(),c()};
}

struct c2 {
    c2() = default;
    c2(const c2&) =default;
    c2(c2&&) = default;
};
void foo2(){
    std::vector<c2> vi = {c2(),c2(),c2()};
}

struct c3 {
    c3() {};
};
void foo3(){
    std::vector<c3> vi = {c3(),c3(),c3()};
}

struct c4 {
    c4() = default;
    c4(const c4&) {};
};
void foo4(){
    std::vector<c4> vi = {c4(),c4(),c4()};
}

struct c5 {
    c5() = default;
    c5(const c5&) {};
    c5(c5&&) = default;
};
void foo5(){
    std::vector<c5> vi = {c5(),c5(),c5()};
}

struct c6 {
    c6() {}
};
void foo6(){
    std::vector<c6> vi = {c6(),c6(),c6()};
}

struct c7 {
    c7() = default;
    c7(const c7&) =default;
};
void foo7(){
    std::vector<c7> vi = {c7(),c7(),c7()};
}
// { dg-final { scan-tree-dump-not "delete" "optimized" } }
