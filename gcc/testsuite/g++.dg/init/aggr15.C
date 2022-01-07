// PR c++/65591

struct ss {
    ss() {};
};
struct C {
      ss s[1000];
};
int main() {
      C cs[5] = {};
}
