// PR c++/28274

extern "C" {
void foo (int i, int j = 6);
void foo (int i = 4, int j);
}
