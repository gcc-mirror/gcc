------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      ADA.CONTAINERS.RED_BLACK_TREES                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

package Ada.Containers.Red_Black_Trees is
pragma Pure (Red_Black_Trees);

   type Color_Type is (Red, Black);

   generic
      type Node_Access is private;
   package Generic_Tree_Types is
      type Tree_Type is record
         First  : Node_Access;
         Last   : Node_Access;
         Root   : Node_Access;
         Length : Count_Type;
      end record;
   end Generic_Tree_Types;
end Ada.Containers.Red_Black_Trees;
