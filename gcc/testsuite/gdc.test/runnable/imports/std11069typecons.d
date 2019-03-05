module imports.std11069typecons;

import imports.std11069array;

template Tuple(Specs...)
{
    struct Tuple
    {
        Specs expand;

        string toString()
        {
            expand[0].empty;
            expand[0].front;
            expand[0].popFront();

            return null;
        }
    }
}


struct RefCounted(T)
{
    T payload;

    ~this()
    {
        //.destroy(_refCounted._store._payload);
        //auto init = typeid(payload).init();
        payload.toString(); // refer Tuple.toString symbol?
    }

}
