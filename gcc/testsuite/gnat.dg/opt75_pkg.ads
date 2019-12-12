pragma Restrictions (No_Abort_Statements);
pragma Restrictions (Max_Asynchronous_Select_Nesting => 0);

with Ada.Finalization;
with System.Atomic_Counters;

package Opt75_Pkg is

  type Rec is record
    Counter : System.Atomic_Counters.Atomic_Counter;
  end record;

  type Rec_Ptr is access all Rec;

  Empty_Rec : aliased Rec;

  type T is new Ada.Finalization.Controlled with record
    Ref : Rec_Ptr := Empty_Rec'Access;
  end record;

  overriding procedure Adjust (Object : in out T);

  Empty : constant T := (Ada.Finalization.Controlled with Ref => Empty_Rec'Access);

  type Arr is array (Integer range 1 .. 8, Integer range 1 .. 4) of T;

end Opt75_Pkg;
