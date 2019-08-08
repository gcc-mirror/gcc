// { dg-do assemble  }
// GROUPS passed error-messages
class foo {};
~foo () {}// { dg-error "1:declaration of .~ foo. as non-member" }  destructors must be member functions.*
