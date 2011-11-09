// PR c++/16489
// { dg-do compile { target c++98 } }

const int NULL = 0;
int main() { 
  double* p = NULL;
}
