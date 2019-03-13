// PR c++/81311
// { dg-do compile { target c++11 } }

struct function
{
  template<class F> function(F) { }
};

struct ref
{
  operator function&() const = delete;
} r;

struct val
{
  operator function() const = delete;
} v;

int main()
{
  function f1(r);
  function f2(v);
}
