// https://issues.dlang.org/show_bug.cgi?id=22388

void setTimer(void delegate()) @system;
void setTimer(void delegate() @safe) @safe;

void setTimer2(void delegate() @safe) @safe;
void setTimer2(void delegate()) @system;

void main() @safe
{
    setTimer(() => assert(false));

    alias lambda = () => assert(false);
    setTimer(lambda);

    // Reversed order

    setTimer2(() => assert(false));

    alias lambda2 = () => assert(false);
    setTimer2(lambda2);
}
