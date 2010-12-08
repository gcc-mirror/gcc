// { dg-options "-w" }

class QString { // { dg-error "previous definition" }
  QString (const QString & a); // { dg-message "QString::QString|candidate expects" }
};

class QString { }; // { dg-error "redefinition" }

const QString q () {
  QString z; // { dg-error "matching" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 10 }
  int x;
  return x ? QString () : QString (); // { dg-error "matching" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 13 }
}
