// PR c++/21113
// { dg-options "" }

void
f (int n)
{
  goto label; // { dg-message "from here" }
  int a[n]; // { dg-message "crosses initialization" }
label: // { dg-error "jump to label" }
  ;
}

void
g (int n)
{
  switch (1)
  {
  case 1:
    int (*a)[n]; // { dg-message "crosses initialization" }
  default: // { dg-error "jump to case label" }
    ;
  }
}
