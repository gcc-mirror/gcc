// PR c++/15076

struct Y { Y(int &); }; // { dg-error "" }

int v;
Y y1(reinterpret_cast<int>(v));  // { dg-error "" }
