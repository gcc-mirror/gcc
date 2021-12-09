// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

module ddoc9305;

/**
foo()
*/
void foo(alias p = (a => a))() {}

/* ret / prm / body */
/*  _  /  _  / expr */  template X(alias pred = x => x) {}                                  ///
/*  _  /  _  / stmt */  template X(alias pred = (x){ int y; return y; }) {}                 /// ditto
/*  _  /  x  / expr */  template X(alias pred = (int x) => x) {}                            /// ditto
/*  _  /  x  / stmt */  template X(alias pred = (int x){ int y; return y; }) {}             /// ditto
/*  x  /  _  / expr */
/*  x  /  _  / stmt */
/*  x  /  x  / expr */
/*  x  /  x  / stmt */

/*  _ /   _  / expr */  template X(alias pred = function (x) => x) {}                       ///
/*  _ /   _  / stmt */  template X(alias pred = function (x){ return x + 1; }) {}           /// ditto
/*  _  /  x  / expr */  template X(alias pred = function (int x) => x) {}                   /// ditto
/*  _  /  x  / stmt */  template X(alias pred = function (int x){ return x + 1; }) {}       /// ditto
/*  x  /  _  / expr */  template X(alias pred = function int(x) => x) {}                    /// ditto
/*  x  /  _  / stmt */  template X(alias pred = function int(x){ return x + 1; }) {}        /// ditto
/*  x  /  x  / expr */  template X(alias pred = function int(int x) => x) {}                /// ditto
/*  x  /  x  / stmt */  template X(alias pred = function int(int x){ return x + 1; }) {}    /// ditto

/*  _ /   _  / expr */  template X(alias pred = delegate (x) => x) {}                       ///
/*  _ /   _  / stmt */  template X(alias pred = delegate (x){ return x + 1; }) {}           /// ditto
/*  _  /  x  / expr */  template X(alias pred = delegate (int x) => x) {}                   /// ditto
/*  _  /  x  / stmt */  template X(alias pred = delegate (int x){ return x + 1; }) {}       /// ditto
/*  x  /  _  / expr */  template X(alias pred = delegate int(x) => x) {}                    /// ditto
/*  x  /  _  / stmt */  template X(alias pred = delegate int(x){ return x + 1; }) {}        /// ditto
/*  x  /  x  / expr */  template X(alias pred = delegate int(int x) => x) {}                /// ditto
/*  x  /  x  / stmt */  template X(alias pred = delegate int(int x){ return x + 1; }) {}    /// ditto
