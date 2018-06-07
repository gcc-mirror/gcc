// PR c++/81311
// { dg-do link }

struct function
{
  template<class F> function(F) { }
};

struct ref
{
  operator function&() const;
} r;

struct val
{
  operator function() const;
} v;

int main()
{
  function f1(r);
  function f2(v);
}
