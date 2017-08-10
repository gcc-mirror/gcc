/* { dg-do compile { target x86_64-*-* } } */

__attribute__((target("default")))
int foo() {return 1;}

__attribute__((target("arch=core2", "")))
int foo2() {return 2;} /* { dg-warning "empty string in attribute .target." } */

__attribute__((target("sse4.2", "", "")))
int foo3() {return 2;} /* { dg-warning "empty string in attribute .target." } */

int main() {
    return foo() + foo2() + foo3();
}
