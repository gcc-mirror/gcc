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
    long B;
    void *pY;
  } ;
  union Y {
    long B;
    void *pY;
  } PRT;
  int PRT_A;
private:
  union {
    long C;
    void *pZ;
  };
  union Z {
    long C;
    void *pZ;
  } PRV;
  int PRV_A;
};

struct Bar : public Foo {
public:
  DoSomething() {
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
    PRV_A = 0;			// ERROR - 
    Foo::C = 0;			// ERROR - 
    printf("%x\n",pZ);  	// ERROR - 
    Foo::PRV.C = 0;		// ERROR - 
    printf("%x\n",PRV.pZ); 	// ERROR - 
  }
};

main()
{
  Foo a;

  a.PUB_A = 0;
  a.A = 0;
  printf("%x\n",a.pX);  
  a.PRT_A = 0;			// ERROR - 
  a.B = 0;			// ERROR - 
  printf("%x\n",a.pY);  	// ERROR - 
  a.PRV_A = 0;			// ERROR - 
  a.C = 0;			// ERROR - 
  printf("%x\n",a.pZ);  	// ERROR - 
  a.PUB.A = 0;
  printf("%x\n",a.PUB.pX);  
  a.PRT.B = 0;			// ERROR - 
  printf("%x\n",a.PRT.pY);  	// ERROR - 
  a.PRV.C = 0;			// ERROR - 
  printf("%x\n",a.PRV.pZ);  	// ERROR - 
}
