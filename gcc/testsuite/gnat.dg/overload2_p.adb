--  { dg-options "-gnat95 -gnatws" }

package body overload2_p is
   function "=" (this, that: t) return boolean is begin return True; end;
   this, that : t;
end;
