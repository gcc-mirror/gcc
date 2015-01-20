// PR c++/58671
// { dg-do compile { target c++11 } }
// { dg-require-effective-target tls }

thread_local int i = i;
