// { dg-do assemble  }
// Origin: Dima Volodin <dvv@dvv.org>

class C {
  static int const N (1000); // { dg-error "" } invalid declaration
};
