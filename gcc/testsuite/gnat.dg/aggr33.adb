-- { dg-do compile }
-- { dg-options "-gnat2022" }

with Ada.Containers.Vectors;

procedure Aggr33 is
   type Light_Count is new Natural;
   subtype Light_Position is Light_Count;

   generic
     Lights : Light_Position;

   package Generic_Machine
   is

      subtype Light_State is Character
        with Static_Predicate => Light_State in '.' | '#';

      Off : constant Light_State := '.';
      Lit : constant Light_State := '#';

      type Panel_Type is
        array (Light_Position range 0 .. Lights - 1) of Light_State;

      Off_Panel : constant Panel_Type := [others => Off];

      -----------------
      -- Toggle_List --
      -----------------

      type Button_Index is new Positive;

      package Toggle_Lists is new
         Ada.Containers.Vectors (Index_Type   => Button_Index,
                                 Element_Type => Light_Position);

      subtype Toggle_List is Toggle_Lists.Vector;

      ----------------
      -- Press_List --
      ----------------

      package Press_Lists is new
         Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Toggle_Lists.Vector,
                                 "="          => Toggle_Lists."=");

      subtype Press_List is Press_Lists.Vector;

      --------------------
      -- Press_Sequence --
      --------------------

      subtype Press_Number is Natural;

      type Press_Outcome is
         record
            Press : Toggle_List;
            Panel : Panel_Type;
         end record;

      package Outcome_Lists is new
         Ada.Containers.Vectors (Index_Type   => Press_Number,
                                 Element_Type => Press_Outcome);

      subtype Outcome_List is Outcome_Lists.Vector;

      --------------
      -- Sequence --
      --------------

      subtype Sequence_Id is Positive;

      package Sequence_Lists is new
         Ada.Containers.Vectors (Index_Type   => Sequence_Id,
                                 Element_Type => Outcome_Lists.Vector,
                                 "="          => Outcome_Lists."=");

      subtype Sequence_List is Sequence_Lists.Vector;

   end Generic_Machine;

   package body Generic_Machine
   is

      procedure Add_Press
      is
         New_Sequence : Sequence_List;
         Toggle       : Toggle_List;
         New_Panel    : Panel_Type;
      begin
          New_Sequence.Append (Outcome_List'(Toggle, New_Panel)); -- { dg-error "too many components" }
      end Add_Press;

   end Generic_Machine;

begin
   null;
end;
