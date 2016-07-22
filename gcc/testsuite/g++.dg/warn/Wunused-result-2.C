// PR c++/66177

struct QSize
{
    QSize(int w, int h) : wd(w), ht(h) {}

    QSize expandedTo() const __attribute__ ((__warn_unused_result__))
    {
        return QSize(2, 3);
    }

private:
    int wd;
    int ht;
};

void foo()
{
    QSize sz(2, 2);
    sz.expandedTo();		// { dg-warning "warn_unused_result" }
}
