// prms-id: 4623

class base {
public:
  int b_data;
  base( int i=0 ) { b_data = i; }
  void b_print() { }
};

class base1: virtual public base {
public:
  int b1_data;
  base1( int i = 0 ) { b1_data = i; b_data++; }
  void b1_print() { }
};

class base2: virtual public base {
public:
  int b2_data;
  base2( int i = 0 ) { b2_data = i; b_data++; }
  void b2_print() { }
};

class base3: public base {};

class derived: public base3, public base1, public base2 {
public:
  int d_data;
  derived( int i ) { d_data = i; ((base3 *)this)->b_data++; }
  void d_print() { }
};

int main() {
  derived d(1); d.d_print(); return 0;
}
