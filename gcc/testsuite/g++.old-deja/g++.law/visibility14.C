// Build don't link: 
// GROUPS passed visibility
// visibility file
// From: ajp@eng.cam.ac.uk
// Date:     Tue, 13 Jul 93 17:15:11 BST
// Message-ID: <1171.9307131615@ace.eng.cam.ac.uk

class A {
    static A* list;

  protected:
    struct AA {
        AA();
        ~AA();
    };
};

A::AA::~AA()
{
    A* d=list;
}
