// { dg-do assemble  }
// GROUPS passed error-messages
class foo {};
~foo () {}// { dg-error "" }  destructors must be member functions.*
