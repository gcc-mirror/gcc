module issue21905;

struct Conv
{
    StaticIterable b;
    alias b this;
}

struct StaticIterable
{
    static Conv b;
    alias b this;
}

void each(ref StaticIterable r)
{
    return ;
}

void main()
{
    StaticIterable s;
    each(s);
}
