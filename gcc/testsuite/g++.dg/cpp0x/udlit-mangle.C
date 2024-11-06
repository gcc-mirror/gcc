// PR c++/52521
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "_Zli2_wPKc" } }

int operator ""_w(const char*);
int main() {
  123_w;
}
