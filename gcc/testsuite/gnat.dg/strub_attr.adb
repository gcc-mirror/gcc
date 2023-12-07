--  { dg-do compile }
--  { dg-options "-fstrub=strict -fdump-ipa-strubm -fdump-ipa-strub" }
--  { dg-require-effective-target strub }

package body Strub_Attr is
   E : exception;

   procedure P (X : Integer) is
   begin
      raise E;
   end;
   
   function F (X : Integer) return Integer is
   begin
      return X * X;
   end;
   
   function G return Integer is (F (X));
   --  function G return Integer is (FP (X));
   --  Calling G would likely raise an exception, because although FP
   --  carries the strub at-calls attribute needed to call F, the
   --  attribute is dropped from the type used for the call proper.
end Strub_Attr;

--  { dg-final { scan-ipa-dump-times "\[(\]strub \[(\]internal\[)\]\[)\]" 2 "strubm" } }
--  { dg-final { scan-ipa-dump-times "\[(\]strub \[(\]at-calls\[)\]\[)\]" 0 "strubm" } }
--  { dg-final { scan-ipa-dump-times "\[(\]strub\[)\]" 1 "strubm" } }

--  { dg-final { scan-ipa-dump-times "strub.watermark_ptr" 6 "strub" } }
--  We have 1 at-calls subprogram (F) and 2 wrapped (P and G).
--  For each of them, there's one match for the wrapped signature, 
--  and one for the update call.

--  { dg-final { scan-ipa-dump-times "strub.watermark" 27 "strub" } }
--  The 6 matches above, plus:
--  5*2: wm var decl, enter, call, leave and clobber for each wrapper;
--  2*1: an extra leave and clobber for the exception paths in the wrappers.
--  7*1: for the F call in G, including EH path.
