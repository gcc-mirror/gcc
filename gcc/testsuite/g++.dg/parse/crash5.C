// { dg-options "-w" }

class QString { // { dg-error "" }
  QString (const QString & a); // { dg-error "" }
};

class QString { }; // { dg-error "" }

const QString q () {
  QString z; // { dg-error "" }
  int x;
  return x ? QString () : QString (); // { dg-error "" }
}
