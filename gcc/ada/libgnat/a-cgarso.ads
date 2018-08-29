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

   with function "<" (Left, Right : Element_Type)
      return Boolean is <>;

procedure Ada.Containers.Generic_Array_Sort (Container : in out Array_Type);

pragma Pure (Ada.Containers.Generic_Array_Sort);
