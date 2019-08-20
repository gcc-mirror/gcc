------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--    A D A . C O N T A I N E R S . G E N E R I C _ A R R A Y _ S O R T     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

generic
   type Index_Type is (<>);
   type Element_Type is private;
   type Array_Type is array (Index_Type range <>) of Element_Type;

   with function "<" (Left, Right : Element_Type) return Boolean is <>;

procedure Ada.Containers.Generic_Array_Sort (Container : in out Array_Type);
pragma Pure (Ada.Containers.Generic_Array_Sort);
--  Reorders the elements of Container such that the elements are sorted
--  smallest first as determined by the generic formal "<" operator provided.
--  Any exception raised during evaluation of "<" is propagated.
--
--  The actual function for the generic formal function "<" is expected to
--  return the same value each time it is called with a particular pair of
--  element values. It should not modify Container and it should define a
--  strict weak ordering relationship: irreflexive, asymmetric, transitive, and
--  in addition, if x < y for any values x and y, then for all other values z,
--  (x < z) or (z < y).  If the actual for "<" behaves in some other manner,
--  the behavior of the instance of Generic_Array_Sort is unspecified. The
--  number of times Generic_Array_Sort calls "<" is unspecified.
