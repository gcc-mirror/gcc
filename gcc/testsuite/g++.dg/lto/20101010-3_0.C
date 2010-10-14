// { dg-lto-do link }
// { dg-lto-options { "-flto -std=c++0x" } }

decltype(nullptr) a;
int main() { return 0; }
