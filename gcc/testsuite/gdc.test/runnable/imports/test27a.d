module imports.test27a;

import std.variant;

class myClass(T) {
public:
    void func(T v) {
        Variant b = Variant(v);
    }
}

