// Build don't link: 
// GROUPS passed conversions
// cvt file
// Message-Id: <1166.9307131600@ace.eng.cam.ac.uk>
// From: ajp@eng.cam.ac.uk
// Date: Tue, 13 Jul 93 17:00:44 BST


class B {};

class A {
  public:
    operator const B*() const {
        return b;
    }
  private:
    B* b;
};


int main()
{
    A a;
    if (a!=0) {
    }
}

