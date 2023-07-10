// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=110471
// { dg-do compile }
// { dg-options "-fno-exceptions" }
version (D_Exceptions)
    static assert(0);
