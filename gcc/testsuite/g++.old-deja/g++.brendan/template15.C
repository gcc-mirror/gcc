// { dg-do assemble  }
// GROUPS passed templates
template<class T> class Stack {
  public:
    Stack (int s = 10);         //Comment out "= 10" and it will compile
    ~Stack(void);               //Omitting "void" doesn't help
}; 

template<class T> Stack<T>::~Stack(void) 
{ }

//If this definition comes before the one for ~Stack, the error message
//about redeclaration of `void Stack<int>::~Stack()' will not occur.
template<class T> Stack<T>::Stack(int s)
{ }

int main () {
    Stack<int> stk(10);
}
