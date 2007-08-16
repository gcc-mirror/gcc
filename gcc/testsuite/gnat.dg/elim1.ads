pragma Eliminate (p, d);
package elim1 is
   type t is tagged null record;
   procedure d (a : t);       
end;
