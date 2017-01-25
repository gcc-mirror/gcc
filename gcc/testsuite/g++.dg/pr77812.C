// PR77812
// struct-stat hack failure when first overload is a template

enum f {};

template <typename>
void f ()
{
}
enum f F;

struct g {};

template <typename>
void g ()
{
}
struct g G;
