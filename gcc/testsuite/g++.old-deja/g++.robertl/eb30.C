// Build don't link:
#include <fstream.h>

class bifstream : public ifstream {
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
