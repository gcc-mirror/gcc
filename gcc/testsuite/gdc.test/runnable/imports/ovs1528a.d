module imports.ovs1528a;

auto func1528() { return 1; }
auto func1528(T)(T) if (is(T : real)) { return 2; }

auto bunc1528(T)(T) if (is(T : real)) { return 2; }
auto bunc1528() { return 1; }

auto vunc1528(int) { return 1; }
auto wunc1528(T)(T[]) { return 2; }

auto opUnary1528(string op : "+")(int) { return 1; }
