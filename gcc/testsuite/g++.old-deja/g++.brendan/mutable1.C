// { dg-do assemble  }
// GROUPS passed mutable
class foo;
class bar {
  mutable foo const *foobar;
};
