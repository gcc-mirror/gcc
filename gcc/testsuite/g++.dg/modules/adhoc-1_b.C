// { dg-additional-options "-fmodules-atom -fdiagnostics-show-caret" }

import bob;
void foo ()
{
  massivelongnamethatcausesadhoclocationsokeepaddingcharsyourgettheidea ();
}

// { dg-regexp "\[^\n]*adhoc-1_b.C:6:74: error: no matching function for call to 'massivelongnamethatcausesadhoclocationsokeepaddingcharsyourgettheidea\\(\\)'\n   massivelongnamethatcausesadhoclocationsokeepaddingcharsyourgettheidea \\(\\);\n                                                                          \\^\n" }
// { dg-regexp "In module bob, imported at \[^\n]*adhoc-1_b.C:3:\n\[^\n]*adhoc-1_a.C:5:12: note: candidate: 'int massivelongnamethatcausesadhoclocationsokeepaddingcharsyourgettheidea\\(int\\)'\n export int massivelongnamethatcausesadhoclocationsokeepaddingcharsyourgettheidea \\(int\\);\n            \\^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n" }
// { dg-regexp "In module bob, imported at \[^\n]*adhoc-1_b.C:3:\n\[^\n]*adhoc-1_a.C:6:188: note: candidate: 'void massivelongnamethatcausesadhoclocationsokeepaddingcharsyourgettheidea\\(float\\)'\n                                                                                                                                                                                export void massivelongnamethatcausesadhoclocationsokeepaddingcharsyourgettheidea \\(float\\);\n                                                                                                                                                                                            \\^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n" }
