// { dg-additional-options "-fmodules-ts -fdiagnostics-show-caret" }

import bob;
void foo ()
{
  massivelongnamethatcausesadhoclocationsokeepaddingcharsyourgettheidea ();
}

// { dg-regexp "\n\[^\n]*adhoc-1_b.C:6:73: error: no matching function for call to 'massivelongnamethatcausesadhoclocationsokeepaddingcharsyourgettheidea\\(\\)'\n   massivelongnamethatcausesadhoclocationsokeepaddingcharsyourgettheidea \\(\\);\n   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\\^~$" }
// { dg-regexp "\nIn module bob, imported at \[^\n]*adhoc-1_b.C:3:\n\[^\n]*adhoc-1_a.C:5:12: note: candidate: 'int massivelongnamethatcausesadhoclocationsokeepaddingcharsyourgettheidea@bob\\(int\\)'\n export int massivelongnamethatcausesadhoclocationsokeepaddingcharsyourgettheidea \\(int\\);\n            \\^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~$" }
// { dg-regexp "\nIn module bob, imported at \[^\n]*adhoc-1_b.C:3:\n\[^\n]*adhoc-1_a.C:6:188: note: candidate: 'void massivelongnamethatcausesadhoclocationsokeepaddingcharsyourgettheidea@bob\\(float\\)'\n\[ \t]*export void massivelongnamethatcausesadhoclocationsokeepaddingcharsyourgettheidea \\(float\\);\n\[ \t]*\\^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~?$" }
// For some reason dg-regexp inserts a blank line 
// { dg-allow-blank-lines-in-output 1 }
