// { dg-do assemble  }
// $7.1.2 disallows explicit on anything but declarations of
// constructors ... including friends.
class foo { public: foo(); };
class bar { public: friend explicit foo::foo(); }; // { dg-error "" } explicit friend
