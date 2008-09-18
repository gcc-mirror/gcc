// { dg-options "-w" }

class QString { // { dg-error "previous definition" }
  QString (const QString & a); // { dg-message "candidates" }
};

class QString { }; // { dg-error "redefinition" }

const QString q () {
  QString z; // { dg-error "matching" }
  int x;
  return x ? QString () : QString (); // { dg-error "matching" }
}
