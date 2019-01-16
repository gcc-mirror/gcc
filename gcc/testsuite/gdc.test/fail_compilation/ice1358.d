/*
TEST_OUTPUT:
---
fail_compilation/ice1358.d(29): Error: invalid UTF character \U80000000
---
*/

// Issue 1358 - ICE(root.c) on Unicode codepoints greater than 0x7FFFFFFF

/* 1358. Assertion failure: '0' on line 1548 in file '..\root\root.c'
This one is trivial.
PATCH(lexer.c, Lexer::escapeSequence()).
--- lexer.c (revision 24)
+++ lexer.c (working copy)
@@ -1281,8 +1281,10 @@
                            break;
                        }
                     }
-                    if (ndigits != 2 && !utf_isValidDchar(v))
+                        if (ndigits != 2 && !utf_isValidDchar(v)) {
                         error("invalid UTF character \\U%08x", v);
+                        v = 0; // prevent ICE
+                        }
                     c = v;
                 }
                 else

*/
auto bla = "\U80000000";
