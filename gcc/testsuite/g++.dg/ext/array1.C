// PR c++/13574
// { dg-options "" }

class A { 
public: 
  A() : argc(0), argv() { }; 	// { dg-error "flexible array" }
private: 
  int argc; 
  char* argv[]; 
}; 
 
int main() { 
  A y; 
} 
