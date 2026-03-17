// PR c++/124227
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -fopenmp" }
// { dg-require-effective-target fopenmp }

using b = float;
template <typename> class c {};
#pragma omp declare reduction(d : c<float> : omp_in)
auto e = [] {
c<b> acc;
#pragma omp parallel reduction(d : acc)
{
}
};

