// PR c++/32080

void f()
try
  {
    goto l2;       // { dg-message "from here" }
  l1: ;            // { dg-error "jump to label 'l1'" }
  } catch (...)
  {
  l2: ;            // { dg-error "jump to label 'l2'" }
                   // { dg-message "enters catch block" "" { target *-*-*} .-1 }
    goto l1;       // { dg-message "from here|enters try block" }
  }
