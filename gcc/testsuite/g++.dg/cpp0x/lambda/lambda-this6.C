// PR c++/53137
// { dg-do compile { target c++11 } }

template <typename STORE>
void getParent(STORE& tStore)
{
}

struct  Store
{
    template <typename CheckParentFunc>
    void updateChildCommon(CheckParentFunc c)
    {
        c();
    }

    template <typename T>
    int& getStore();

    template <typename T>
    void updateChild(const T& obj)
    {
        updateChildCommon([this] () { getParent(getStore<T>()); });
    }

    void update(int obj);
};

void Store::update(int obj)
{
    updateChild(obj);
}
