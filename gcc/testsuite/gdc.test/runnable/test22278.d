/*
REQUIRED_ARGS: -release
PERMUTE_ARGS:  -check=in=on -check=out=on
*/

// https://issues.dlang.org/show_bug.cgi?id=22278

bool resultIn;
bool resultOut;

void foo22278()
    in { resultIn = true; }
    out { resultOut = true; }
do {}

int main()
{
    foo22278();

    version(D_PreConditions)  assert(resultIn);  else assert(!resultIn);
    version(D_PostConditions) assert(resultOut); else assert(!resultOut);

    return 0;
}
