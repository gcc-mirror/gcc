// { dg-do compile }
// { dg-options "-fno-exceptions" }

typedef unsigned int uint;
struct QShared {
    bool deref() {
	return !--count;
    }
    uint count;
};
template <class T> class QValueListNode {
public:
    QValueListNode<T>* next;
    QValueListNode<T>* prev;
};
template <class T> class QValueListPrivate : public QShared {
public:
    typedef QValueListNode<T> Node;
    typedef QValueListNode<T>* NodePtr;
    QValueListPrivate();
    void derefAndDelete()     {
	if ( deref() )      delete this;
    }
    ~QValueListPrivate();
    NodePtr node;
};
template <class T>  QValueListPrivate<T>::QValueListPrivate() {
    node = new Node;
    node->next = node->prev = node;
}
template <class T>  QValueListPrivate<T>::~QValueListPrivate() {
    NodePtr p = node->next;
    while( p != node ) {
	NodePtr x = p->next;
	delete p;
	p = x;
    }
}
template <class T> class QValueList {
public:
    QValueList() {
	sh = new QValueListPrivate<T>;
    }
    ~QValueList() {
	sh->derefAndDelete();
    }
    QValueListPrivate<T>* sh;
};
class Cell {
    QValueList<Cell*> obscuringCells() const;
};
QValueList<Cell*> Cell::obscuringCells() const {
    QValueList<Cell*> empty;
}
