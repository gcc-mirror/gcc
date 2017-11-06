/* { dg-do compile } */
/* { dg-additional-options "-Wno-return-type" } */

typedef int PRInt32;
class nsTreeRows {
    class Subtree { };
    enum { kMaxDepth = 32 };
    struct Link {
        Subtree* mParent;
        PRInt32 mChildIndex;
        Link&         operator=(const Link& aLink) {
            mParent = aLink.mParent;
            mChildIndex = aLink.mChildIndex;
        }
    };
    class iterator {
        PRInt32 mTop;
        PRInt32 mRowIndex;
        Link mLink[kMaxDepth];
    public:
        iterator() : mTop(-1), mRowIndex(-1) { }
        iterator& operator=(const iterator& aIterator);
    };
    Subtree*     EnsureSubtreeFor(Subtree* aParent, PRInt32 aChildIndex);
    Subtree*     GetSubtreeFor(const Subtree* aParent,
PRInt32 aChildIndex,                   PRInt32* aSubtreeSize = 0);
    void     InvalidateCachedRow() {
        mLastRow = iterator();
    }
    iterator mLastRow;
};
nsTreeRows::Subtree* nsTreeRows::EnsureSubtreeFor(Subtree* aParent,
                     PRInt32 aChildIndex) {
    Subtree* subtree = GetSubtreeFor(aParent, aChildIndex);
    if (! subtree) {
        InvalidateCachedRow();
    }
}
nsTreeRows::iterator& nsTreeRows::iterator::operator=(const iterator&
aIterator) {
    mTop = aIterator.mTop;
    for (PRInt32 i = mTop;
         i >= 0;
         --i)         mLink[i] = aIterator.mLink[i];
}
