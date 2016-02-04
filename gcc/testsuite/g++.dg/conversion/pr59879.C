// PR c++/59879

struct Test {
 template <int N>
 Test(const char (&array)[N]) {}
};

Test test() {
 return "test1";
}

void test2(Test arg = "test12") {}

template <typename T>
void test3(T arg = "test123") {}

template <typename T>
void test4(const T &arg = "test123") {}

int main() {
 test();
 test2();
 test3<Test>();
 test4<Test>();
}
