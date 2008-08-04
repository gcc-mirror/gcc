--  { dg-do run }

procedure BIP_Aggregate_Bug is

   package Limited_Types is

      type Lim_Tagged is tagged limited record
         Root_Comp : Integer;
      end record;

      type Lim_Ext is new Lim_Tagged with record
         Ext_Comp : Integer;
      end record;

      function Func_Lim_Tagged (Choice : Integer) return Lim_Tagged'Class;

   end Limited_Types;

   package body Limited_Types is

      function Func_Lim_Tagged (Choice : Integer) return Lim_Tagged'Class is
      begin
         case Choice is
            when 111 =>
               return Lim_Ext'(Root_Comp => Choice, Ext_Comp => Choice);
            when 222 =>
               return Result : Lim_Tagged'Class
                        := Lim_Ext'(Root_Comp => Choice, Ext_Comp => Choice);
            when others =>
               return Lim_Tagged'(Root_Comp => Choice);
         end case;
      end Func_Lim_Tagged;

   end Limited_Types;

   use Limited_Types;

   LT_Root : Lim_Tagged'Class := Func_Lim_Tagged (Choice => 999);
   LT_Ext1 : Lim_Tagged'Class := Func_Lim_Tagged (Choice => 111);
   LT_Ext2 : Lim_Tagged'Class := Func_Lim_Tagged (Choice => 222);

begin
   if LT_Root.Root_Comp /= 999
     or else Lim_Ext (LT_Ext1).Ext_Comp /= 111
     or else Lim_Ext (LT_Ext2).Ext_Comp /= 222
   then
      raise Program_Error;
   end if;
end BIP_Aggregate_Bug;
