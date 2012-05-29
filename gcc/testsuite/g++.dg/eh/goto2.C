// PR c++/32080

void f()
try
  {
    goto l2;       // { dg-error "from here" }
  l1: ;            // { dg-error "jump to label 'l1'" }
  } catch (...)
  {
  l2: ;            // { dg-error "jump to label 'l2'|enters catch block" }
    goto l1;       // { dg-error "from here|enters try block" }
  }
