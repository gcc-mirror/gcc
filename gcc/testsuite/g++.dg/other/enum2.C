// PR c++/51248

enum E { e = sizeof(const E*) };
