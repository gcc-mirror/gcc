with overload2_q;
package overload2_p is
   type t is new overload2_q.t;
private
   function "=" (this, that: t) return boolean;
end;
