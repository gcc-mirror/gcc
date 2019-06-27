/* { dg-do compile } */

enum e1 { A = 256 } __attribute__((__mode__(__byte__))); /* { dg-error "specified mode too small for enumerated values" } */
enum e2 { B = 256 } __attribute__((__packed__, __mode__(__byte__))); /* { dg-error "specified mode too small for enumerated values" } */

enum e3 { C = __INT_MAX__ } __attribute__((__mode__(__QI__))); /* { dg-error "specified mode too small for enumerated values" } */
enum e4 { D = __INT_MAX__ } __attribute__((__packed__, __mode__(__QI__))); /* { dg-error "specified mode too small for enumerated values" } */

enum e5 { E = __INT_MAX__ } __attribute__((__mode__(__HI__))); /* { dg-error "specified mode too small for enumerated values" "" { xfail int16 } } */
enum e6 { F = __INT_MAX__ } __attribute__((__packed__, __mode__(__HI__))); /* { dg-error "specified mode too small for enumerated values" "" { xfail int16 } } */
