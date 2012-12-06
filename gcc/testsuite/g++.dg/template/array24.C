// PR c++/55032

template<typename T>
struct vec3t {
  T c[3];
};

typedef vec3t<float> vec3;

class Bounds {
  public:
    Bounds(const vec3 bb[2]);
    void foo(const vec3 & v) { v.c[0]; }
};

template<typename T>
void work(T& value);

void foo() {
  vec3 bb[2];
  work(bb);
}
