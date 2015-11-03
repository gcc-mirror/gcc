/* { dg-do compile } */

/* Tests iterators are allowed in ordered loops and that we keep track
   of the original iterator DECL for diagnostic purposes.  */

#include <iostream>
#include <vector>

/* Test plain iterator.  */
void foo1 ()
{
  std::vector<int> v;
  for (int i=1; i<=5; i++) v.push_back(i);

  std::vector<int>::const_iterator it;

#pragma omp parallel for ordered(1)
  for (it = v.begin(); it < v.end(); ++it)
    {
#pragma omp ordered depend(sink:it-1)
    std::cout << *it << '\n';
#pragma omp ordered depend(source)
    }
}

/* Test non-dependent iterator in a template.  */
template <int N>
void foo2 ()
{
  std::vector<int> v;
  for (int i=1; i<=5; i++) v.push_back(i);

  std::vector<int>::const_iterator it;
#pragma omp parallel for ordered(1)
  for (it = v.begin(); it < v.end(); ++it)
    {
#pragma omp ordered depend(sink:it-1)
    std::cout << *it << '\n';
#pragma omp ordered depend(source)
    }
}

/* Test dependent iterator in a template.  */
template <typename T>
void foo3 ()
{
  std::vector<T> v;
  for (int i=1; i<=5; i++) v.push_back(i);

  typename std::vector<T>::const_iterator it;
#pragma omp parallel for ordered(1)
  for (it = v.begin(); it < v.end(); ++it)
    {
#pragma omp ordered depend(sink:it-1)
    std::cout << *it << '\n';
#pragma omp ordered depend(source)
    }
}  

int main ()
{
  foo2 <0> ();
  foo3 <int> ();
}
