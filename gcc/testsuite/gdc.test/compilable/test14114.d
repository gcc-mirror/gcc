// REQUIRED_ARGS: -O

// https://issues.dlang.org/show_bug.cgi?id=14114

import core.volatile;

struct Ports {
    static ubyte B() { return volatileLoad(cast(ubyte *)0x0025); }
    static void B(ubyte value) { volatileStore(cast(ubyte *)0x0025, value); }
}
