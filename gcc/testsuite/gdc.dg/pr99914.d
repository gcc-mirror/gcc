// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=99914
// { dg-additional-options "-fmain" }
// { dg-do link { target d_runtime } }

extern(C) __gshared bool rt_cmdline_enabled = false;
