/* REQUIRED_ARGS: -betterC
 */

// https://issues.dlang.org/show_bug.cgi?id=23606

string foo()()
{
    string a, b;
    return a ~ b;
}

enum s = foo();
