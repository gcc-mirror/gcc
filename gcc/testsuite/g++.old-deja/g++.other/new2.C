//Build don't link:
typedef __SIZE_TYPE__ size_t;
void *operator new(size_t, unsigned int);

struct X{
  X();
  X(int);
};


void f(X *x = new X);          // ERROR - 

void f(X *x = new X(4));       // ERROR - 

void f(X *x = new X[4]);       // ERROR - 

void f(X *x = new (3) X(6));   // ERROR - 

void f(X *x = new (2) X[10]);  // ERROR - 

void f(X *x = new X[10][5]);   // ERROR - 
