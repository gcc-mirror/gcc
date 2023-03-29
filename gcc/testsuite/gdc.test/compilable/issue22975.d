// https://issues.dlang.org/show_bug.cgi?id=22975
void test22975a(int) {};

alias test22975b = test22975a;

void test22975b(bool) {}

alias test22975c = test22975b;

alias test22975a = test22975c;

void test22975c(float) {}
