// { dg-do assemble  }
// GROUPS passed static-mem
struct test {
  void test_member() {
    static test& ds = *this;    // FIX: static test* ds = this;
  }
};


int main()
{
  test t;
}
