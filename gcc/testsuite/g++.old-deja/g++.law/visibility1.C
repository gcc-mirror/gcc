// Build don't link: 
// GROUPS passed visibility
#include <iostream>



class base {
//==========

    void base_priv(char * n) 		
	{ std::cout << "base_priv called from: " << n << "\n";  };

protected:

    void base_prot(char * n) 
	{ std::cout << "base_prot called from: " << n << "\n"; };

public:

    void base_publ(char * n) 
	{ std::cout << "base_publ called from: " << n << "\n"; };

    void test(char * n) { base_publ(n); base_prot(n); base_priv(n); }

}; // class base
 


class derived : private base {	// Make this public, 
//============================	// and we don't get an error

friend void derived_friend();

public :

    void test(char * n) { base_publ(n); base_prot(n);}

}; // class derived



void
derived_friend()
//--------------
{
    derived pd;

    pd.base_publ("friend of derived class");	// Compiler error here
    pd.base_prot("friend of derived class");
}



int main(int argc, char *argv[])
//==========================
{
    base b;
    b.base_publ("base class object");
    b.test("member of base class object");
    std::cout << "\n";

    derived pd;
    pd.test("member of derived class object");
    derived_friend();
    std::cout << "\n";

} /* main */

