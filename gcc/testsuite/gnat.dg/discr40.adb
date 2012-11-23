-- { dg-do compile }
-- { dg-options "-gnat12 -gnata" }

package body Discr40 is

   procedure Push (S: in out Stack; E : Element) is
   begin
      S.Length := S.Length + 1;
      S.Data(S.Length) := E;
   end Push;

end Discr40;
