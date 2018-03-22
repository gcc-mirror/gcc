-- { dg-do compile }
-- { dg-options "-gnatws" }

with System.Multiprocessors;

procedure Atomic10 is

  type Atomic_Unsigned is mod 2 ** 32;
  pragma Atomic (Atomic_Unsigned);

  Max : Positive := Positive (System.Multiprocessors.Number_Of_CPUs);

  Comp_Size : constant := 64 * 8;

  subtype Index_Type is Positive range 1 .. Max;

  type Array_Type is array (Index_Type) of aliased Atomic_Unsigned; -- { dg-error "cannot be guaranteed" }
  for Array_Type'Component_Size use Comp_Size;

  Slots : Array_Type;
begin
  for Index in Index_Type loop
     Slots (Index) := 0;
   end loop;
end;
