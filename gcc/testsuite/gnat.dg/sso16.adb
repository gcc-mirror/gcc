--  { dg-do run }

with Ada.Text_IO; use Ada.Text_IO;

procedure SSO16 is

   pragma Default_Scalar_Storage_Order (High_Order_First);

   type Enum_T is
     (Event_0,
      Event_1,
      Event_2,
      Event_3,
      Event_4,
      Event_5,
      Event_11,
      Event_12,
      Event_13,
      Event_14,
      Event_15,
      Event_21,
      Event_22,
      Event_23,
      Event_24,
      Event_25,
      Event_31,
      Event_32,
      Event_33,
      Event_34,
      Event_35,
      Event_41,
      Event_42,
      Event_43,
      Event_44,
      Event_45);

   Var : Enum_T := Event_0;

begin
   if Var'Image /= "EVENT_0" then
      raise Program_Error;
   end if;

   if Enum_T'Value ("Event_4")'Image /= "EVENT_4" then
      raise Program_Error;
   end if;

   if Enum_T'Val (20)'Image /= "EVENT_35" then
      raise Program_Error;
   end if;

   if Enum_T'Pos (Enum_T'Value ("Event_45"))'Image /= " 25" then
      raise Program_Error;
   end if;
end;
