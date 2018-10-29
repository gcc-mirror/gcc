/*
TEST_OUTPUT:
---
fail_compilation/ice14929.d(45): Error: cast(Node)(*this.current).items[this.index] is not an lvalue
fail_compilation/ice14929.d(88): Error: template instance ice14929.HashMap!(ulong, int).HashMap.opBinaryRight!"in" error instantiating
fail_compilation/ice14929.d(92):        instantiated from here: HashmapComponentStorage!int
fail_compilation/ice14929.d(92): Error: template instance ice14929.isComponentStorage!(HashmapComponentStorage!int, int) error instantiating
fail_compilation/ice14929.d(92):        while evaluating: `static assert(isComponentStorage!(HashmapComponentStorage!int, int))`
---
*/

struct HashMap(K, V)
{
    V* opBinaryRight(string op)(K key) const if (op == "in")
    {
        size_t index;
        foreach (ref node; buckets[index].range)
        {
            return &(node.value);
        }
        return null;
    }

    struct Node
    {
        K key;
        V value;
    }

    alias Bucket = UnrolledList!(Node);
    Bucket[] buckets;
}

struct UnrolledList(T)
{
    Range range() const pure
    {
        return Range(_front);
    }

    static struct Range
    {
        ref T front() const @property
        {
            return cast(T) current.items[index];
        }
        void popFront() pure
        {
        }
        bool empty() const pure @property
        {
            return true;
        }
        const(Node)* current;
        size_t index;
    }

    Node* _front;

    static struct Node
    {
        ContainerStorageType!T[10] items;
    }
}

template ContainerStorageType(T)
{
    alias ContainerStorageType = T;
}

template isComponentStorage(CS, C)
{
    enum bool isComponentStorage = is(typeof(
    (inout int = 0)
    {
        CS cs = CS.init;
        ulong eid;
        cs.add(eid, c);
    }));
}

struct HashmapComponentStorage(ComponentType)
{
    private HashMap!(ulong, ComponentType) components;

    void add(ulong eid, ComponentType component)
    {
        assert(eid !in components);
    }
}

static assert(isComponentStorage!(HashmapComponentStorage!int, int));

void main()
{
}
