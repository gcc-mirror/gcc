// { dg-do compile }
// { dg-options "-std=gnu++0x" }

template<typename T>
struct list {};

template<typename T>
struct vector { 
  operator T() const;
};

void f()
{
  vector<vector<int>> v;
  const vector<int> vi = static_cast<vector<int>>(v);
}
