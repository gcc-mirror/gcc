// { dg-options "-std=c++0x" }
enum class Color1 {
  Red, 
  Green,
  Blue
};

enum struct Color2 {
  Red, // { dg-error "previously declared here" }
  Orange,
  Yellow,
  Green,
  Blue,
  Indigo = Green + 2,
  Violet,
  Red // { dg-error "redefinition" }
};

enum Color {
  Red, Green, Blue
};

enum class Color3 {
  Red
};

enum class Color color;
enum Color3 color3;

void f(int);
void f2(Color3);

void g()
{
  int i = 0;
  f(color); // okay: unscoped enum
  f(color3); // { dg-error "cannot convert" }
  f2(color); // { dg-error "cannot convert" }
  f2(color3);
  f2(i);     // { dg-error "cannot convert" }
  i = color3; // { dg-error "cannot convert" }
  color3 = i; // { dg-error "cannot convert" }
  f(static_cast<int>(color3)); // okay

  int a[5];
  a[color3]; // { dg-error "array subscript is not an integer" }

  bool b = color3; // { dg-error "cannot convert" }
}

void h()
{
  Color1 c1 = Color1::Red;
  Color2 c2 = Color1::Red; // { dg-error "cannot convert" }
  c2 = Color1::Red; // { dg-error "cannot convert" }

  c2 = Color2::Red;
  int c3 = Color::Red;
}

template<typename T, T value>
struct constant { };

template<typename T>
int& sfinae(constant<T, T::Green>*);

float& sfinae(void*);

void sfinae_test()
{
  int& test1 = sfinae((constant<Color1, Color1::Green>*)0);
  int& test2 = sfinae((constant<Color2, Color2::Green>*)0);
  float& test3 = sfinae((constant<Color1, Color1::Red>*)0);
  int& test4 = sfinae((constant<Color, Green>*)0);
  float& test5 = sfinae((constant<Color, Red>*)0);
}
