// { dg-do assemble  }
// PRMS Id: 9127
// Bug: members of anonymous unions are not access-controlled.

#include <stdio.h>

struct Foo {
public:
  union {
    long A;
    void *pX;
  };
  union X {
    long A;
    void *pX;
  } PUB ;
  int PUB_A;
protected:
  union {
    long B; // { dg-error "" } protected
    void *pY; // { dg-error "" } protected
  } ;
  union Y {
    long B;
    void *pY;
  } PRT; // { dg-error "" } protected
  int PRT_A; // { dg-error "" } protected
private:
  union {
    long C; // { dg-error "" } private
    void *pZ; // { dg-error "" } private
  };
  union Z {
    long C;  
    void *pZ;
  } PRV; // { dg-error "" } private
  int PRV_A; // { dg-error "" } private
};

struct Bar : public Foo {
public:
  void DoSomething() {
    PUB_A = 0;
    Foo::A = 0;
    printf("%x\n",pX);  
    Foo::PUB.A = 0;
    printf("%x\n",PUB.pX);  
    B = 0;			
    printf("%x\n",Foo::pY);  
    PRT_A = 0;
    PRT.B = 0;		
    printf("%x\n",Foo::PRT.pY);	
    PRV_A = 0;			// { dg-error "" } 
    Foo::C = 0;			// { dg-error "" } 
    printf("%x\n",pZ);  	// { dg-error "" } 
    Foo::PRV.C = 0;		// { dg-error "" } 
    printf("%x\n",PRV.pZ); 	// { dg-error "" } 
  }
};

int main()
{
  Foo a;

  a.PUB_A = 0;
  a.A = 0;
  printf("%x\n",a.pX);  
  a.PRT_A = 0;			// { dg-error "" } 
  a.B = 0;			// { dg-error "" } 
  printf("%x\n",a.pY);  	// { dg-error "" } 
  a.PRV_A = 0;			// { dg-error "" } 
  a.C = 0;			// { dg-error "" } 
  printf("%x\n",a.pZ);  	// { dg-error "" } 
  a.PUB.A = 0;
  printf("%x\n",a.PUB.pX);  
  a.PRT.B = 0;			// { dg-error "" } 
  printf("%x\n",a.PRT.pY);  	// { dg-error "" } 
  a.PRV.C = 0;			// { dg-error "" } 
  printf("%x\n",a.PRV.pZ);  	// { dg-error "" } 
}
