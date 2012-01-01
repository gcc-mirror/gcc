// PR c++/15076

struct Y { Y(int &); };

int v;
Y y1(reinterpret_cast<int>(v));
