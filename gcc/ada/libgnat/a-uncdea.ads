------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           A D A . U N C H E C K E D _ D E A L L O C A T I O N            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This package is defined by ARM 13.11.2 to implement unchecked storage
--  deallocation of an object designated by a value of an access type.

generic
   type Object (<>) is limited private;
   type Name is access Object;

procedure Ada.Unchecked_Deallocation (X : in out Name) with
  Depends => (X    => null,  --  X on exit does not depend on its input value
              null => X),    --  X's input value has no effect
  Post => X = null;          --  X's output value is null
--  Deallocate object denoted by X, and set X to null.

pragma Preelaborate (Unchecked_Deallocation);

pragma Import (Intrinsic, Ada.Unchecked_Deallocation);
