--  { dg-do compile }
package body Iface_Test is
   protected SQLite_Safe is
      function Prepare_Select
        (DB   : DT_1;
         Iter : Standard.Iface_Test.Iface_2'Class)
      return Standard.Iface_Test.Iface_2'Class;
   end;

   overriding procedure Prepare_Select
     (DB   : DT_1;
      Iter : in out Standard.Iface_Test.Iface_2'Class)
   is
   begin
      Iter := SQLite_Safe.Prepare_Select (DB, Iter);  --  test
   end;

   protected body SQLite_Safe is
      function Prepare_Select
        (DB   : DT_1;
         Iter : Standard.Iface_Test.Iface_2'Class)
        return Standard.Iface_Test.Iface_2'Class
      is
      begin
         return Iter;
      end;
   end;
end;
