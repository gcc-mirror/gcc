// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for fstream" { ! hostedlib } }
#include <fstream>

class bifstream : public std::ifstream {
public:
    bifstream();
//     ~bifstream();
};

void load_bin()
{
    bifstream InFile;

    if (!InFile)
        return;
}
