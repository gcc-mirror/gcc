//Build don't link:
namespace A{
}

struct Y: A::S<int>{};     //ERROR - no such type
