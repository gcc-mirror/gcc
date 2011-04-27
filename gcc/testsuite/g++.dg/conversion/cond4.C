// Origin: PR c++/45383
// { dg-do run }

struct null {
    null() {}
    template<class T>
    operator T*() const {
    return 0;
    }

    template<class C, class T>
    operator T C::*() const {
    return 0;
    }
private:
    null(const null&);
    null& operator=(const null&);
    void operator&() const;
};

static struct null null;

int
main()
{
    int* ptr = null;
    if (ptr == null)
        return 0;
    if (ptr != null)
        return 1;
}
