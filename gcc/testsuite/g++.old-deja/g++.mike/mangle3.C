// { dg-do run  }
struct ZZ {
  int p;
};


template <int ZZ::* ptr>
struct YY {
  ZZ qq;
  YY() { }
};

int main() {
  YY<&ZZ::p> ww;
}
