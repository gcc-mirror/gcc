// https://issues.dlang.org/show_bug.cgi?id=21661

module pkg.pkg2.mod;

immutable x = 1;
enum e = pkg.pkg2.mod.x;

// Some checks
static assert(           pkg.stringof == "package pkg" );
static assert(      pkg.pkg2.stringof == "package pkg2");
static assert(  pkg.pkg2.mod.stringof == "module mod"  );
static assert(pkg.pkg2.mod.x.stringof == "x"           );

alias p1 = pkg;
alias p2 = pkg.pkg2;
alias m  = pkg.pkg2.mod;
alias v  = pkg.pkg2.mod.x;

static assert(         p1.stringof == "package pkg" );
static assert(         p2.stringof == "package pkg2");
static assert(          m.stringof == "module mod"  );
static assert(p1.pkg2.mod.stringof == "module mod"  );
static assert(     p2.mod.stringof == "module mod"  );
static assert(          v.stringof == "x"           );
