// PR c++/10245

struct X {};
    
struct Y {
  Y ();
  operator X () const;
private:
  Y (const Y &);
};

Y y;
void foo() {
  X x = (1 ? Y() : Y());
}
