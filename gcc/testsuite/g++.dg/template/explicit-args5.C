// PR c++/51613

template<typename F, typename T>
void apply(F f, T t)
{
    f(t);
}

template<typename T>
void multi(T)
{
}

template<typename T>
void multi(T*)
{
}

int main()
{
    apply(&multi<int>, 7);	// { dg-error "no match" }

    return 0;
}
