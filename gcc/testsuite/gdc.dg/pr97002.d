// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=97002
// { dg-do compile }

import core.stdc.stdarg;

enum E1 : bool { one, two }
enum E2 : short { one, two }
enum E3 : dchar { one, two }
enum E4 : float { one, two }

extern(C) void fun(void *p1, ...)
{
    va_arg!bool(_argptr);   // { dg-warning "'bool' is promoted to 'int' when passed through '...'" "promoted" }
    // { dg-message "note: .so you should pass .int. not .bool. to .va_arg.." "int not bool" { target *-*-* } .-1 }
    // { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } .-2 }

    va_arg!byte(_argptr);   // { dg-warning "'byte' is promoted to 'int' when passed through '...'" "promoted" }
    // { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } .-1 }

    va_arg!ubyte(_argptr);   // { dg-warning "'ubyte' is promoted to 'int' when passed through '...'" "promoted" }
    // { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } .-1 }

    va_arg!short(_argptr);   // { dg-warning "'short' is promoted to 'int' when passed through '...'" "promoted" }
    // { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } .-1 }

    va_arg!ushort(_argptr);   // { dg-warning "'ushort' is promoted to 'int' when passed through '...'" "promoted" }
    // { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } .-1 }

    va_arg!char(_argptr);   // { dg-warning "'char' is promoted to 'int' when passed through '...'" "promoted" }
    // { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } .-1 }

    va_arg!wchar(_argptr);   // { dg-warning "'wchar' is promoted to 'int' when passed through '...'" "promoted" }
    // { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } .-1 }

    va_arg!dchar(_argptr);   // { dg-warning "'dchar' is promoted to 'uint' when passed through '...'" "promoted" }
    // { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } .-1 }

    va_arg!float(_argptr);   // { dg-warning "'float' is promoted to 'double' when passed through '...'" "promoted" }
    // { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } .-1 }

    va_arg!ifloat(_argptr);   // { dg-warning "'ifloat' is promoted to 'idouble' when passed through '...'" "promoted" }
    // { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } .-1 }

    va_arg!E1(_argptr);   // { dg-warning "'E1' is promoted to 'int' when passed through '...'" "promoted" }
    // { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } .-1 }

    va_arg!E2(_argptr);   // { dg-warning "'E2' is promoted to 'int' when passed through '...'" "promoted" }
    // { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } .-1 }

    va_arg!E3(_argptr);   // { dg-warning "'E3' is promoted to 'uint' when passed through '...'" "promoted" }
    // { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } .-1 }

    va_arg!E4(_argptr);   // { dg-warning "'E4' is promoted to 'double' when passed through '...'" "promoted" }
    // { dg-message "note: if this code is reached, the program will abort"  "will abort" { target *-*-* } .-1 }
}
