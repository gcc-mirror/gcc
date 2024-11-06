void swap(long &a, long &b)
{
  long t = a;
  a = b;
  b = t;
}

struct Array {
    long arr[1];
    Array() : arr() {}
    /* Operators */
    long& operator[](int index) { return arr[index]; }
    const long& operator[](int index) const { return arr[index]; }
    /* Operations */
    void swap(Array& array)  {
        for (int i = 0; i < 1; ++i)
            ::swap(arr[i], array[i]);
    }
};

class Vector : public Array {};

struct v
{
  Vector *e;
  v() : e (new Vector[4]){}
  Vector& operator[](int index) { return e[index]; }
  const Vector& operator[](int index) const { return e[index]; }
};
static inline Vector func(const Vector& y)
{
        return y;
}

volatile int a;

int main() {
    v solution;
    solution[0][0] = 1;
    int t = a;
    for (int i = 0; i < 3; ++i) {
        const Vector& v = solution[i];
        Vector sum;
        const long delta = func(v)[0] & t;
        sum[0] = v[0] + delta;
        solution[i + 1].swap(sum);
    }
    for(int i = 0; i < 4; i++)
    {
      if (solution[i][0] != 1)
        __builtin_abort();
    }
    return 0;
}
