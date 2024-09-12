// { dg-do run  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
// PRMS Id: 8569

#include <iostream>
#include <vector>
#include <cstdlib>

using std::vector;
 
class Component {
    int george;
    char mabel[128];
};
class CopyMe {
public:
    CopyMe(){;}
private:
    vector<Component> strvec;
};

class IncludeIt   {
public:
    IncludeIt() {}
    ~IncludeIt() {}
    IncludeIt(const IncludeIt& i) {
        myStrvec = i.myStrvec;
    }
    IncludeIt& operator=(const IncludeIt& i) {
	myStrvec = i.myStrvec;
	return *this;
    }
private:
    CopyMe myStrvec;
};

int main(int argc, char**argv)   {
    IncludeIt foo;
    IncludeIt* bar;
    std::exit(0);
}
