// Build don't link: 
// GROUPS passed warnings
// there should be a warning about foo only defining private methods
class foo {
  int bar();
};// ERROR - .*
