struct Container { int Count(); };
struct List : private Container {
    using Container::Count;
};
struct INetContentTypeParameterList : private List { void Clear(); };
void INetContentTypeParameterList::Clear() {
    Count();//Calling non static but in a non-static method.
}
