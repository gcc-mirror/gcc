// Build don't link:

template <class S, class T>
class mem_fun1_t {
public:
  mem_fun1_t(S (T::*pf)(double)) {} 
};

template <class T>
class mem_fun1_t<void, T> { 
public:
  mem_fun1_t(void (T::*pf)(double)) {}
};

struct Operation {
  double eval(double) { return 0; }
};

int main() {
  mem_fun1_t<double, Operation> m(&Operation::eval);
}
