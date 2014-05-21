typedef __SIZE_TYPE__ size_t;
void *operator new(size_t, unsigned int);

struct X{
  X();
  X(int);
};


void f(X *x = new X);          // { dg-message "" } 

void f(X *x = new X(4));       // { dg-error "" } 

void f(X *x = new X[4]);       // { dg-error "" } 

void f(X *x = new (3) X(6));   // { dg-error "" } 

void f(X *x = new (2) X[10]);  // { dg-error "" } 

void f(X *x = new X[10][5]);   // { dg-error "" } 
