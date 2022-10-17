alias U = const ubyte[uint.sizeof]*;
static assert (is(U == const(ubyte[4]*)));
