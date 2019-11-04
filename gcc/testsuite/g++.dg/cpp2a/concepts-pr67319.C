// PR c++/67319
// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

template <typename T>
concept bool Any()
{
  return requires (T t) { +t; };
}

struct my_struct
{
  template <Any... Args>
  auto sample(Args... args) -> void;
};

int main()
{
  my_struct{}.sample();
  my_struct{}.sample(0);
  my_struct{}.sample(0, 'a');
  my_struct{}.sample(nullptr); // { dg-error "" }
}

