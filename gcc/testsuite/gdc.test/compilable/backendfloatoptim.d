// REQUIRED_ARGS: -O -inline

//https://issues.dlang.org/show_bug.cgi?id=20143
real fun(int x) { return 0.0; }

double bug()
{
    // value passed to fun is irrelevant
    return 0.0 / fun(420);
}
