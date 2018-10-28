module imports.ice10857b;

import imports.ice10857a;
import imports.ice10857b;

void foo(int outer)
{
    int[] infos = [1];
    auto f1 = filter!(s => outer)(infos);     // NG: error is triggered here
    f1.popFront();
    auto f2 = filter!((int s)=>outer)(infos); // OK
    f2.popFront();
}
void main() { foo(123); }
