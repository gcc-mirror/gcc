// Build don't link: 
// GROUPS passed operators
// opr-as file
// From: Klaus Ahrens <ahrens@informatik.hu-berlin.de>
// Date:     Fri, 26 Mar 93 12:50:37 mez
// Subject:  no default assignment
// Message-ID: <199303261149.TA23114@mail.Germany.EU.net>

class A {
public:
        A(int){}
        A& operator=(const A&){return *this;}
};

class B: public A {
public:
        B(int i): A(i) {}
};

int main()
{
        B b=1;
        b=1;
}
