// Build don't link: 
// GROUPS passed error-messages
class foo {};
~foo () {}// ERROR -  destructors must be member functions.*
