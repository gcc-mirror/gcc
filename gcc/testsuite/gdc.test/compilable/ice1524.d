// Issue 1524 - ICE(constfold.c) on using "is" with strings in CTFE

/* 1524 PATCH Assertion failure: '0' on line 863 in file 'constfold.c'
constfold.c
@@ -845,9 +845,9 @@
     Loc loc = e1->loc;
     int cmp;

-    if (e1->op == TOKnull && e2->op == TOKnull)
+    if (e1->op == TOKnull || e2->op == TOKnull)
     {
-        cmp = 1;
+        cmp = (e1->op == TOKnull && e2->op == TOKnull) ? 1 : 0;
     }
     else if (e1->op == TOKsymoff && e2->op == TOKsymoff)
     {
*/
bool isNull(string str)
{
    return str is null;
}
const bool test = isNull("hello!");
