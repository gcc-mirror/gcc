-- { dg-do compile }
-- { dg-options "-O" }

with Ada.Unchecked_Deallocation;

with Opt46_Pkg;

package body Opt46 is

   type Pattern is abstract tagged null record;

   type Pattern_Access is access Pattern'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Pattern'Class, Pattern_Access);

   type Action is abstract tagged null record;

   type Action_Access is access Action'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Action'Class, Action_Access);

   type Pattern_Action is record
      Pattern : Pattern_Access;
      Action  : Action_Access;
   end record;

   package Pattern_Action_Table is new Opt46_Pkg (Pattern_Action, Natural, 1);

   type Session_Data is record
      Filters : Pattern_Action_Table.Instance;
   end record;

   procedure Close (Session : Session_Type) is
      Filters : Pattern_Action_Table.Instance renames Session.Data.Filters;
   begin
      for F in 1 .. Pattern_Action_Table.Last (Filters) loop
         Free (Filters.Table (F).Pattern);
         Free (Filters.Table (F).Action);
      end loop;

   end Close;

end Opt46;
