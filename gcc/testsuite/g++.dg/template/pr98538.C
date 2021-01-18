// PR c++/98538
// { dg-do compile { target c++11 } }
// ICE bulding a dependent array type variant

template<typename T> using A = int[1];
template<typename T, const A<T>> struct X { };

template<typename T>
void
f (const A<T>)
{
  const A<T> a;
}

template<typename T>
struct Y {
  const A<T> a;
};
