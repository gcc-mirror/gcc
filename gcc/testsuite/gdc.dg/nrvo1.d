// { dg-do compile }
// { dg-additional-options "-fpreview=dip1000" }
ThreadInfo* ptr;

ThreadInfo receiveOnly()
{
    ThreadInfo ret;

    get({ptr = &ret;});
    return ret;
}

struct ThreadInfo
{
    ThreadInfo* next;
}

bool get(T)(T)
{
    return false;
}

void main()
{
    auto t = receiveOnly();
    assert(&t == ptr);
}
