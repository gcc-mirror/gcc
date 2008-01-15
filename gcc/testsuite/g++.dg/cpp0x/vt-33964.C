// { dg-options "-std=c++0x" }
template<typename ... Args>
struct foo
{
    static bool const value = true;
};

template<typename ... Args>
struct foo< typename Args::is_applied... > // { dg-error "not used|Args" }
{
    static bool const value = false;
};

struct not_applied { typedef void is_applied; };
struct applied { typedef applied is_applied; };

int main()
{
    foo<applied, applied> i;
}
