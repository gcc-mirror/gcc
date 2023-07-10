// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=110471
// { dg-do compile }
// { dg-options "-fno-rtti" }
version (D_TypeInfo)
    static assert(0);
