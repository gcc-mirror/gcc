// PR c++/81607

int a;

struct b {
  long c : 32;
} d;

char f = (903092 ? int(d.c) : 0) << a;
