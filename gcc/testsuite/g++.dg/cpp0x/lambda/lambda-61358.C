// { dg-do compile { target c++11 } }

int main() {
  goto doit;
  []() { doit: ; };
  doit: ;
}
