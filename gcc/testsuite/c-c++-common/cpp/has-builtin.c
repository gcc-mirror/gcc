/* PR c/66970 - Add __has_builtin() macro
   Verify that errors are detected and handled gracefully.
   { dg-do compile } */

#ifndef __has_builtin
#  error "__has_builtin is not defined"
#endif

#if __has_builtin             // { dg-error "missing '\\\(' after \"__has_builtin\"" }
#endif

#if __has_builtin (           // { dg-error "macro \"__has_builtin\" requires an identifier" }
#endif

#if __has_builtin ()          // { dg-error "macro \"__has_builtin\" requires an identifier" }
#endif

#if __has_builtin (1)         // { dg-error "macro \"__has_builtin\" requires an identifier" }
#endif

#if __has_builtin (1, 2)      // { dg-error "macro \"__has_builtin\" requires an identifier" }
#endif

#if __has_builtin (1 + 2)     // { dg-error "macro \"__has_builtin\" requires an identifier" }
#endif

#if __has_builtin (x, y)      // { dg-error "expected '\\\)' after \"x\"" } */
#endif

#if __has_builtin (x + 1)     // { dg-error "expected '\\\)' after \"x\"" } */
#endif

#if __has_builtin (p->i)      // { dg-error "expected '\\\)' after \"p\"" } */
#endif

#if __has_builtin ((x))       // { dg-error "macro \"__has_builtin\" requires an identifier" }
#endif

#if __has_builtin ((y)        // { dg-error "macro \"__has_builtin\" requires an identifier" }
#endif

#if __has_builtin ((((z)      // { dg-error "macro \"__has_builtin\" requires an identifier" }
#endif

#if __has_builtin (x)))       // { dg-error "missing '\\\('" }"
#endif

#if __has_builtin (f ())      // { dg-error "expected '\\\)' after \"f\"" }"
#endif
