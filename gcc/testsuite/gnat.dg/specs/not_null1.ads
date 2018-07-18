-- { dg-do compile }

package Not_Null1 is
   type T is null record;
   type T_Access is access all T;

   procedure Proc (This : in not null T_Access) is null;

   type Proc_Access is access procedure (This : in not null T_Access);
   PA : Proc_Access := Proc'Access;
end Not_Null1;
