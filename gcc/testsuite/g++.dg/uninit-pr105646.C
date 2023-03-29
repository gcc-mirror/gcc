// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-options "-O2 -Wuninitialized" }

int f1();
int f2(){
    bool v2{v2}; // { dg-warning "is used uninitialized" }
    auto const & a = f1();
    return a;
}
int f3(){
    auto const & a = f1();
    // Diagnose the following when optimizing and as unconditional
    // uninitialized use despite f1 possibly throwing
    bool v3{v3}; // { dg-warning "is used uninitialized" }
    return a;
}
