// Build don't link: 
// GROUPS passed templates
template <class A, class B> class Map;

class Foo
{
public:
    static Map<int,int> bar;
};

template <class A, class B>
class Map
{
public :
    int find();
};

int main()
{
    int z = Foo::bar.find();
}
