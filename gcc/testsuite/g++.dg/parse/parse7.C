/* PR c++/3650 */
/* { dg-do compile } */

class class1 {
public:
  explicit class1(double a) { data = a;  }
  double data;
};

class class2 {
public:
  class2(class1 a, float t) {  }
  class2(float t, class1 a) {  }
};

int main() {
  float t2 = 1.5;
  double pir = 3.14159;
  // Used to get: error: type specifier omitted for parameter `t2'
  class2 h(class1(double(pir)), t2);
  class2 i(class1(pir), t2);

  return 0;
}
