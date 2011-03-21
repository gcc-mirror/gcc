/* PR c++/30917 */
/* This used to ICE */
/* { dg-do compile } */


class QGList;
unsigned count() {
  class QGListIterator {
    friend class QGList;
    QGListIterator( const QGList & ); // OK, finds ::QGList.
  };
  return 0;
}

// This is valid.
unsigned count2() {
  class QGList2;
  class QGListIterator2 {
    friend class QGList2;
    QGListIterator2( const QGList2 & );
  };
  return 0;
}
