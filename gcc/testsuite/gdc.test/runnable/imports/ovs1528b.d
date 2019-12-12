module imports.ovs1528b;

auto func1528(string) { return 3; }
auto func1528(T)(T[]) { return 4; }

auto bunc1528(T)(T[]) { return 4; }
auto bunc1528(string) { return 3; }

auto vunc1528(T)(T[]) { return 2; }
auto wunc1528(int) { return 1; }

auto opUnary1528(string op : "-")(int) { return 2; }
