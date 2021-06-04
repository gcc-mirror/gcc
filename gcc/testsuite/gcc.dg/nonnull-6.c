/* PR c/100783 - ICE on -Wnonnull and erroneous type
   { dg-do compile }
   { dg-options "-Wall" } */

__attribute__((nonnull (1))) void
f1 (char[][n]);                         // { dg-error "undeclared" }

__attribute__((nonnull (2))) void
f2 (int n, char[n][m]);                 // { dg-error "undeclared" }

__attribute__((nonnull (1))) void
f3 (char[*][n]);                        // { dg-error "undeclared" }

__attribute__((nonnull (1))) void
f4 (char[f1]);                          // { dg-error "size" }
