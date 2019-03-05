--  { dg-do run }

procedure Array33 is
  generic
    type Item_T is private; -- The type of which the interval is made of.
    type Bound_T is private;
    None_Bound : Bound_T;
    Bounds_Are_Static : Boolean := False;
    type Value_T is private;
    type Base_Index_T is range <>;
  package General_Interval_Partition_G is
    subtype Length_T is Base_Index_T range 0 .. Base_Index_T'Last;
    subtype Index_T  is Base_Index_T range 1 .. Base_Index_T'Last;
    type T is private;
    function Single (First, Last : Bound_T; Value : Value_T) return T;
    function Single1 (First, Last : Bound_T; Value : Value_T) return T;
  private
    type Bounds_Array_T is array (Length_T range <>) of Bound_T;
    type Values_Array_T is array (Index_T  range <>) of Value_T;

    First_Bounds_Index : constant Length_T
        := 2 * Boolean'Pos (Bounds_Are_Static);
    -- See below explanation on indexing the bounds.


    type Obj_T (Length : Length_T) is
      record
        Bounds : Bounds_Array_T (First_Bounds_Index .. Length)
           := (others => None_Bound);
        -- This is tricky. If Bounds_Are_Static is true, the array does not
        --  store the lower or upper bound.
        -- This lowers memory requirements for the data structure at the cost
        --  of slightly more complex indexing.
        --
        -- Bounds as seen internally depending on the parameter:
        --
        -- Bounds_Are_Static | Lower_Bound | Inbetween Bounds (if any) | Upper_Bound
        --     True         => Max_First   & Bounds (2 .. Length)      & Min_Last
        --     False        => Bounds (0)  & Bounds (1 .. Length - 1)  & Bounds (Length)
        --
        Values : Values_Array_T (1 .. Length);
      end record;

    type T is access Obj_T;
    --@@ if ccf:defined(debug_pool) then
    --@@! for T'Storage_Pool use Pool_Selection_T'Storage_Pool;
    --@@ end if

  end General_Interval_Partition_G;

  package body General_Interval_Partition_G is

    function Single (First, Last : Bound_T; Value : Value_T) return T is
    begin
      return new Obj_T'(Length => 1,
                        Bounds => (if Bounds_Are_Static
                                   then (2 .. 0 => None_Bound) 
                --  Now raises constraint error here
                                   else (0 => First, 1 => Last)),
                        Values => (1 => Value));
    end Single;
    function Single1 (First, Last : Bound_T; Value : Value_T) return T is
    begin
      return new Obj_T'( 1,
                         (if Bounds_Are_Static
                                   then (2 .. 0 => None_Bound) 
                --  Now raises constraint error here
                                   else (0 => First, 1 => Last)),
                        (1 => Value));
    end Single1;
  end General_Interval_Partition_G;

  type T is new Integer;

  package Partition is new General_Interval_Partition_G (Item_T            => T,
                                                         Bound_T           => T,
                                                         None_Bound        => 0,
                                                         Bounds_Are_Static => True,
                                                         Value_T           => T,
                                                         Base_Index_T      => Natural);
  X : constant Partition.T := Partition.Single (1,1,1);
  Z : constant Partition.T := Partition.Single1 (1,1,1);
begin
  null;
end;
