/*
REQUIRED_ARGS: -Xf- -o- -version=Showme
PERMUTE_ARGS:
TEST_OUTPUT:
----
[
 {
  "kind" : "module",
  "file" : "compilable$?:windows=\\|/$json20742.d",
  "members" : [
   {
    "name" : "X1",
    "kind" : "struct",
    "protection" : "private",
    "line" : 52,
    "char" : 13,
    "members" : []
   },
   {
    "name" : "Y2",
    "kind" : "struct",
    "protection" : "private",
    "line" : 59,
    "char" : 13,
    "members" : []
   },
   {
    "name" : "A1",
    "kind" : "struct",
    "protection" : "private",
    "line" : 62,
    "char" : 13,
    "members" : []
   },
   {
    "name" : "B2",
    "kind" : "struct",
    "protection" : "private",
    "line" : 69,
    "char" : 13,
    "members" : []
   }
  ]
 }
]
----

https://issues.dlang.org/show_bug.cgi?id=20742
*/

version(Showme)
    private struct X1 {}
else
    private struct X2 {}

version(Hideme)
    private struct Y1 {}
else
    private struct Y2 {}

static if (true)
    private struct A1 {}
else
    private struct A2 {}

static if (false)
    private struct B1 {}
else
    private struct B2 {}
