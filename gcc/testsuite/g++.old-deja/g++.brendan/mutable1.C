// Build don't link: 
// GROUPS passed mutable
class foo;
class bar {
  mutable foo const *foobar;
};
