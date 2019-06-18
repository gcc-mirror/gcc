#include <array>
#include <string>
#include <vector>

template<typename T>
T& identity (T& v) { return v; }
template<typename T>
T** identityPP (T** v) { return v; }

template<typename T>
std::vector<T>* getVector(size_t len, const T* ptr)
{
    std::vector<T>* ret = new std::vector<T>(len);
    for (size_t i = 0; i < len; ++i)
        (*ret)[i] = ptr[i];
    return ret;
}

std::string* getString(int len, const char* ptr)
{
    return new std::string(ptr, len);
}

template<typename T, size_t N>
std::array<T, N>* getArray(const T* ptr)
{
    std::array<T, N>* ret = new std::array<T, N>();
    for (size_t x = 0; x < N; ++x)
        (*ret)[x] = ptr[x];
    return ret;
}

// Explicit instantiations, so they are callable outside this compilation unit.
template int** identityPP<int>(int**);
template float** identityPP<float>(float**);
template int& identity<int>(int&);
template float& identity<float>(float&);

template std::vector<int>* getVector<int>(size_t, const int*);
template std::vector<float>* getVector<float>(size_t, const float*);
//typedef std::vector<float> svf_t;
//template std::vector<svf_t>* getVector<svf_t>(size_t, const svf_t*);

template std::array<int, 4>* getArray<int, 4>(const int*);
template std::array<float, 4>* getArray<float, 4>(const float*);
//typedef Foo<int, 42> fi42_t;
//template std::array<fi42_t, 4> getArray<fi42_t, 4>(const fi42_t*);
