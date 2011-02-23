// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/40866
// { dg-options "-std=gnu++98" }
// { dg-do compile }

template <typename T> class QForeachContainer {
public:
    QForeachContainer();
    int brk;
    typename T::const_iterator i;
};

template <typename T> class QList {
public:
    class const_iterator {
    public:
        const_iterator(const const_iterator &o);
        const_iterator &operator++();
    };
};

class QAction;
class QWidget {
public:
    QList<QAction*> actions() const;
};
class myDialog : public QWidget {
    myDialog();
};

myDialog::myDialog()
{
    QForeachContainer<__typeof__(actions())> _container_;
    ({++_container_.brk; ++_container_.i;});
}

