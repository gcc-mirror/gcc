template<int n>
struct tento {
  enum {value = 10*tento<n-1>::value};
};

struct tento<0> { // { dg-error "" }
   enum {value=1};
};

int main() {
  if (tento<4>::value != 10000) return -1;
}

