--  { dg-options "-gnat95" }

package body overload2_q is
  function "=" (this, that: t) return boolean is begin return False; end;
end;
