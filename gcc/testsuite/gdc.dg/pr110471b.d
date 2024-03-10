// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=110471
// { dg-do compile }
// { dg-options "-fno-moduleinfo" }
version (D_ModuleInfo)
    static assert(0);
