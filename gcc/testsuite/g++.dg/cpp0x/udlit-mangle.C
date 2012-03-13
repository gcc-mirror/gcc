// PR c++/52521
// { dg-options -std=c++0x }
// { dg-final { scan-assembler "_Zli2_wPKc" } }

int operator "" _w(const char*);
int main() {
  123_w;
}
