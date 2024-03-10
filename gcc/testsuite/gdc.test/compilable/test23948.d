// https://issues.dlang.org/show_bug.cgi?id=23948

void foo1(const(char)* fun = __FILE__)() {

}

void foo2(const(char)* fun = __FILE_FULL_PATH__)() {

}

void foo3(const(char)* fun = __MODULE__)() {

}

void foo4(const(char)* fun = __FUNCTION__)() {

}

void foo5(const(char)* fun = __PRETTY_FUNCTION__)() {

}

void main() {
    foo1();
    foo2();
    foo3();
    foo4();
    foo5();
}
