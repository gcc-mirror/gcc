// prms-id: 10166

template <class A>
class B {
  public:
    int f() {
      for(int x=0;x<10;x++) {
        continue;
        return 1;
     }
     return 0;
   }
  private:
    A w;
};

main() {
  B<int> c;
  return c.f();
}
