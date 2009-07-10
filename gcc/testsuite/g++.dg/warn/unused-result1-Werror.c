// PR 40614
// { dg-options "-Werror=unused-result" }
class QByteArray {
public:
  QByteArray(const QByteArray &);
};
class QString {
  QByteArray toLocal8Bit() const __attribute__ ((warn_unused_result));
  void fooWarnHere() const { toLocal8Bit(); } // { dg-error "ignoring" }
};
