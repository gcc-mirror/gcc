--  { dg-do compile }
--  { dg-options "-gnat2020" }

procedure Delta_Aggr is
   type T1 is tagged record
      F1, F2, F3 : Integer := 0;
   end record;

   function Make (X : Integer)  return T1 is
   begin
      return (10, 20, 30);
   end Make;

   package Pkg is
      type T2 is new T1 with private;
      X, Y : constant T2;
      function Make (X : Integer) return T2;
   private
      type T2 is new T1 with
         record
            F4 : Integer := 0;
         end record;
      X : constant T2 := (0, 0, 0, 0);
      Y : constant T2 := (1, 2, 0, 0);
   end Pkg;

   package body Pkg is
      function Make (X : Integer) return T2 is
      begin
         return (X, X ** 2, X ** 3, X ** 4);
      end Make;
   end Pkg;

   use Pkg;

   Z : T2 := (Y with delta F1 => 111);

   -- a legal delta aggregate whose type is a private extension
   pragma Assert (Y = (X with delta F1 => 1, F2 => 2));
   pragma assert (Y.F2 = X.F1);

begin
   Z := (X with delta F1 => 1);

   --  The base of the delta aggregate can be overloaded, in which case
   --  the candidate interpretations for the aggregate are those of the
   --  base, to be resolved from context.

   Z := (Make (2) with delta F1 => 1);
   null;
end Delta_Aggr;
