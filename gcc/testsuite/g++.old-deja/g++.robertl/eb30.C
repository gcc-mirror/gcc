// Build don't link:
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
