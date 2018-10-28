// Issue 854 - TypeTuple in anonymous delegate causes ice in glue.c

/* 854 VOTE PATCH (=2863, =2251?) Assertion failure: '0' on line 935 in file 'glue.c'
I haven't checked this patch myself.
--- dmd/func.c  2009-03-05 01:56:46.000000000 +0100
+++ dmd-fixed/func.c        2009-03-30 00:39:41.000000000 +0200
@@ -756,6 +756,27 @@
             }
         }

+        if (f->parameters)
+        {
+            for (size_t i = 0; i < Argument::dim(f->parameters); i++)
+            {
+                Argument *arg = (Argument *)Argument::getNth(f->parameters, i);
+                Type* nw = arg->type->semantic(0, sc);
+                if (arg->type != nw) {
+                    arg->type = nw;
+                    // Examine this index again.
+                    // This is important if it turned into a tuple.
+                    // In particular, the empty tuple should be handled or the
+                    // next parameter will be skipped.
+                    // FIXME: Maybe we only need to do this for tuples,
+                    //        and can add tuple.length after decrement?
+                    i--;
+                }
+            }
+            // update nparams to include expanded tuples
+            nparams = Argument::dim(f->parameters);
+        }
+
         // Propagate storage class from tuple parameters to their element-parameters.
         if (f->parameters)
         {
*/
template Foo(T...)
{
    alias T Foo;
}
void main()
{
    auto y = (Foo!(int) x){ return 0; };
}
