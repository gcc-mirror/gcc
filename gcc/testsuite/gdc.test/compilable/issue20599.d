import core.stdc.config;

enum A : cpp_long;
enum B : cpp_longlong;

enum C : cpp_long { a,b,c };
enum D : cpp_longlong { a,b,c };

enum : cpp_long { a,b,c };
enum : cpp_longlong { d,e,f };
