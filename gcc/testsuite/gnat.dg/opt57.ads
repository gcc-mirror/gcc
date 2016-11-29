-- { dg-do compile }
-- { dg-options "-O3" }

with Ada.Finalization; use Ada.Finalization;
with Opt57_Pkg; use Opt57_Pkg;

package Opt57 is

   procedure Update;

   procedure Init;

   type Module_Factory is abstract new Limited_Controlled with private;

   type Root_Module_Rec (Language : access Module_Factory'Class)
   is abstract new GC_Pool with null record;

   type List is tagged limited private;
   type Linkable is abstract new Root_Module_Rec with private;
   type Linkable_Ptr is access all Linkable'Class;

private

   type Link is access all List'Class;
   type Link_Constant is access constant List'Class;
   type List is tagged limited record
      Next : Link;
   end record;

   type Links_Type (Container : access Linkable) is new List with null record;

   type Linkable is abstract new Root_Module_Rec with record
      On_List : Link_Constant;
      Links   : aliased Links_Type (Linkable'Access);
   end record;

   type Module_Rec (Language : access Module_Factory'Class)
   is abstract new Linkable (Language) with null record;
   type Module_Ptr is access all Module_Rec'Class;

   type Private_Module_Factory;
   type Private_Module_Factory_Ptr is access Private_Module_Factory;

   type Module_Factory is abstract new Limited_Controlled with record
      Priv : Private_Module_Factory_Ptr;
   end record;

   type Module_Factory_Ptr is access all Module_Factory'Class;

end Opt57;
