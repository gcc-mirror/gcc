// PR c++/13574
// { dg-options "" }

class A { 
public: 
  A() : argc(0), argv() { }; 
private: 
  int argc; 
  char* argv[]; 
}; 
 
int main() { 
  A y; 
} 
