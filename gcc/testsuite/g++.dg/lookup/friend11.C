/* PR c++/30917 */
/* This used to ICE */
/* { dg-do "compile" } */


// This is invalid: QGList must only be looked up in count.
class QGList;
unsigned count() {
  class QGListIterator {
    friend class QGList;
    QGListIterator( const QGList & ); /* { dg-error "expected|with no type" } */
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
