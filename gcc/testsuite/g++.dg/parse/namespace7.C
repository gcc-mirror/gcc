namespace O {
  struct SO;
  namespace I {
    struct SI;
    struct O::SO {}; // { dg-error "" }
  }
  struct I::SI {};
}
