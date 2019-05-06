// PR c++/90265
// { dg-do compile { target c++14 } }

void (*a)(int, int, int, void *) = [](auto, auto, auto, auto) {};
