module test16214b;
import test16214a;

struct Appender() { int[] arr; }
struct Tuple() { alias A = Appender!(); }

class EventLoop
{
    auto f() { auto x = [Tuple!().init]; }
}
